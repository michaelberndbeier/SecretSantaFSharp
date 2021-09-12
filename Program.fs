open System

type ParticipantDetails = {Name :string;}
type SenderReceiverPairing = {Sender:ParticipantDetails; Receiver:ParticipantDetails;}
type IndexPairing = {SenderIndex:int;ReceiverIndex:int;} 

let testParticipants = [|{Name="Michael"}; {Name="Alice"}; {Name="Bob"}; 
                   {Name="Charlie"};
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


let calcIndexList numberOfParticipants = 
    let random = System.Random()
    let offset = random.Next(1, (numberOfParticipants - 1))
    [1 .. numberOfParticipants] |> Seq.map (fun index -> {SenderIndex=(index - 1);ReceiverIndex=( calcIndexRelative numberOfParticipants index offset)})


[<EntryPoint>]
let main argv = 
    let indexPairs = calcIndexList (testParticipants |> Seq.length)
    let converted = indexPairs |> Seq.map (fun indexPairing -> indexPairingToPair (indexPairing, testParticipants))
    printfn "%A" converted
    0
