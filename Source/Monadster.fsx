
type VitalForce = {units: int}

let getVitalForce vitalForce =
    let oneUnit = {units = 1}
    let remaining = {units = vitalForce.units - 1}
    oneUnit, remaining


type Label = string

type DeadLeftLeg = DeadLeftLeg of Label

type LiveLeftLeg = LiveLeftLeg of Label * VitalForce

type M<'LiveBodyPart> =
    M of (VitalForce -> 'LiveBodyPart * VitalForce)

let runM (M f) vitalForce = f vitalForce

let makeLiveLeftLegM deadLeftLeg =
    // create an inner intermediate function
    let becomeAlive vitalForce =
        let (DeadLeftLeg label) = deadLeftLeg
        let oneUnit, remainingVitalForce = getVitalForce vitalForce
        let liveLeftLeg = LiveLeftLeg (label,oneUnit)
        liveLeftLeg, remainingVitalForce
    // return it
    M becomeAlive

let deadLeftLeg = DeadLeftLeg "Boris"

let leftLegM = makeLiveLeftLegM deadLeftLeg

let vf = {units = 10}
let liveLeftLeg, remainingAfterLeftLeg = runM leftLegM vf

// -------------------------------------------------------

type DeadLeftBrokenArm = DeadLeftBrokenArm of Label

type LiveLeftBrokenArm = LiveLeftBrokenArm of Label * VitalForce

type LiveLeftArm = LiveLeftArm of Label * VitalForce

type HealBrokenArm = LiveLeftBrokenArm -> LiveLeftArm


let healBrokenArm (LiveLeftBrokenArm (label, vf)) = LiveLeftArm (label, vf)

let makeHealedLeftArm brokenArmM =

    // create a new inner function that takes a vitalForce parameter
    let healWhileAlive vitalForce =
        // run the incoming brokenArmM with the vitalForce
        // to get a broken arm
        let brokenArm,remainingVitalForce = runM brokenArmM vitalForce

        // heal the broken arm
        let healedArm = healBrokenArm brokenArm

        // return the healed arm and the remaining VitalForce
        healedArm, remainingVitalForce

    // wrap the inner function and return it
    M healWhileAlive