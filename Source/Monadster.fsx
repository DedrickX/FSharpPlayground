
type VitalForce = {units: int}

let getVitalForce vitalForce =
    let oneUnit = {units = 1}
    let remaining = {units = vitalForce.units - 1}
    oneUnit, remaining


type Label = string

type DeadLeftLeg = DeadLeftLeg of Label

type LiveLeftLeg = LiveLeftLeg of Label * VitalForce

type MakeLiveLeftLeg =
    DeadLeftLeg -> (VitalForce -> LiveLeftLeg * VitalForce)

type M<'LiveBodyPart> =
    VitalForce -> 'LiveBodyPart * VitalForce

let makeLiveLeftLeg deadLeftLeg : M<LiveLeftLeg> =
    // create an inner intermediate function
    let becomeAlive vitalForce =
        let (DeadLeftLeg label) = deadLeftLeg
        let oneUnit, remainingVitalForce = getVitalForce vitalForce
        let liveLeftLeg = LiveLeftLeg (label,oneUnit)
        liveLeftLeg, remainingVitalForce
    // return it
    becomeAlive

