open System

type ParticipantDetails = {Name :string;}
type SenderReceiverPairing = {Sender:ParticipantDetails; Receiver:ParticipantDetails;}
type IndexPairing = {SenderIndex:int;ReceiverIndex:int;} 

let testParticipants = [|{Name="Michael"}; {Name="Alice"}; {Name="Bob"}; 
                   {Name="Charlie"}; {Name="Elise"}; {Name="Fiona"};
                   {Name="George"}; {Name="Hanna"}; {Name="Ida"}; 
                   {Name="Julian"}; {Name="Luise"};{Name="Kilian"}; |]

let indexPairingToPair (indexPairing:IndexPairing, participants: ParticipantDetails array) = {Sender = participants.[ indexPairing.SenderIndex ]; Receiver = participants.[ indexPairing.ReceiverIndex]}

let calcIndexRelative numberOfParticipants participant offset = 
    let baseAddress = (numberOfParticipants * (participant - 1)) // Set pointer to participant
    let baseOffset = (participant - 1) // Set offset to participant gifts participant
    let combinedOffset = baseOffset + offset // Add run-specific offset
    let fittingOffset = if (combinedOffset >= numberOfParticipants) then combinedOffset - numberOfParticipants else combinedOffset // "warp around"
    ((baseAddress + fittingOffset) % numberOfParticipants)

let isListInvalid (indexList: IndexPairing seq) =  indexList |> Seq.exists( fun indexPairing -> (indexPairing.SenderIndex.Equals( indexPairing.ReceiverIndex)))

let rec calcIndexList numberOfParticipants = 
    let random = System.Random()
    let offset = random.Next(1, (numberOfParticipants - 1))
    let indexList = [1 .. numberOfParticipants] |> Seq.map (fun index -> {SenderIndex=(index - 1);ReceiverIndex=( calcIndexRelative numberOfParticipants index offset)})
    if (isListInvalid indexList) then (calcIndexList numberOfParticipants) else indexList

let calcPairingList participants = calcIndexList (testParticipants |> Seq.length) |> Seq.map (fun indexPairing -> indexPairingToPair (indexPairing, testParticipants))

[<EntryPoint>]
let main argv = 
    printfn "%A" (calcPairingList testParticipants)
    0
