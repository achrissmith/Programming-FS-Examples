// Copyright 2012 Chris Smith. All Rights Reserved.

module Query.Enumerators

open System
open System.Collections.Generic

open Indexer.DataTypes

/// Generaliziation of a seq<TokenPosition> with an AdvanceTo method for
/// bulk advancing. Note that this only performs a one-through traversal of
/// the underlying sequence.
///
/// Intentionally does not implement IEnumerable or IEnumerator because the
/// casting required in F# makes it _very_ painful to deal with.
type TokenPostingList(basePostingList : seq<TokenPosition>) =
    let underlyingEnumerator = basePostingList.GetEnumerator()
    // Move enumerator to initial position and initialize m_atEnd.
    // ... is how you would initialize a normal enumerator. A seq<_> from the F#
    // compiler however appears do not support Reset().
    // do underlyingEnumerator.Reset()
    let mutable m_atEnd = not <| underlyingEnumerator.MoveNext()

    /// If the enumerator as reached its final position. Reset by calling
    member this.AtEnd = m_atEnd

    member this.AdvanceTo(tokenPosition) =
        while (not this.AtEnd) && (this.Current < tokenPosition) do
            this.MoveNext()

    member this.Current =
        if not m_atEnd then
            underlyingEnumerator.Current
        else
            TokenPosition.InvalidTokenPosition

    member this.MoveNext() =
        m_atEnd <- not <| underlyingEnumerator.MoveNext()
        ()

    member this.MoveNextDocument() =
        let startingDoc = this.Current.DocumentID
        while not this.AtEnd && this.Current.DocumentID = startingDoc do
            this.MoveNext()


/// Exact phrase enumerator. Returns all document positions where the given
/// tokens occur in a specific order.
///
/// This is done by finding all hits of the enumerators where the positions
/// differ by one.
let createExactPhraseEnumerator(phraseParts : TokenPostingList[]) =
    new TokenPostingList(
        seq {
            if phraseParts.Length > 0 then
                while not <| phraseParts.[0].AtEnd do
                    let firstTokenPosition = phraseParts.[0].Current

                    // Advance all other enumerators to that point (if possible).
                    for i = 1 to phraseParts.Length - 1 do
                        phraseParts.[i].AdvanceTo(firstTokenPosition)

                    // Check if they all line up.
                    let foldFunc (expectedDocID, expectedPosition, lineUp) (phrasePartEnumerator : TokenPostingList) =
                        let lineUp' = 
                            lineUp &&
                            (phrasePartEnumerator.Current.DocumentID = expectedDocID) &&
                            (phrasePartEnumerator.Current.DocumentPosition = expectedPosition)
                        (expectedDocID, expectedPosition + 1, lineUp')

                    let initialState =
                        (firstTokenPosition.DocumentID, firstTokenPosition.DocumentPosition, true)

                    let allLineUp =
                        phraseParts
                        |> Array.fold foldFunc initialState
                        |> (fun (_, _, lineUp) -> lineUp)

                    if allLineUp then
                        yield firstTokenPosition
                        
                    // Move to the next start of the phrase and continue.
                    phraseParts.[0].MoveNext()
    })

/// Combine two enumerators to return only documents found in both enumerators.
let conjunctiveEnumerator (iter1 : TokenPostingList) (iter2 : TokenPostingList) =
    new TokenPostingList(
        seq {
            while (not iter1.AtEnd) && (not iter2.AtEnd) do
                if iter1.AtEnd then
                    iter2.MoveNextDocument()
                elif iter2.AtEnd then
                    iter1.MoveNextDocument()
                else
                    let i1Current = iter1.Current
                    let i2Current = iter2.Current
                    if i1Current.DocumentID = i2Current.DocumentID then
                        // Take care not to yield the same document more than once.
                        yield i1Current
                        iter1.MoveNextDocument()
                        iter2.MoveNextDocument()
                    elif i1Current < i2Current then
                        yield i1Current
                        iter1.MoveNextDocument()
                    else
                        yield i2Current
                        iter2.MoveNextDocument()
    })

/// Disjunctive enumerator returning documents that are found in either list.
let disjunctiveEnumerator (iter1 : TokenPostingList) (iter2 : TokenPostingList) =
    new TokenPostingList(
        seq {
            while (not iter1.AtEnd) || (not iter2.AtEnd) do
                if iter1.AtEnd then
                    yield iter2.Current
                    iter2.MoveNextDocument()
                elif iter2.AtEnd then
                    yield iter1.Current
                    iter1.MoveNextDocument()
                else
                    let i1Current = iter1.Current
                    let i2Current = iter2.Current
                    if i1Current.DocumentID = i2Current.DocumentID then
                        // Take care not to yield the same document more than once.
                        yield i1Current
                        iter1.MoveNextDocument()
                        iter2.MoveNextDocument()
                    elif i1Current < i2Current then
                        yield i1Current
                        iter1.MoveNextDocument()
                    else
                        yield i2Current
                        iter2.MoveNextDocument()
    })