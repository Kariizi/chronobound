module Abilities where
import Model
import Data.List (minimumBy)
import Data.Ord (comparing)

distanceSquared :: (Float, Float) -> (Float, Float) -> Float

distanceSquared (x1, y1) (x2, y2) =
    (x2 - x1)^2 + (y2 - y1)^2

livingMobs :: World -> [Mob]

livingMobs world =
    filter (\mob -> mobHealth mob > 0) (mobs world)

findNearestMob :: World -> Maybe Mob

findNearestMob world =
    let
        playerPosition = position (player world)

        aliveMobs = livingMobs world

    in
        case aliveMobs of
            [] -> Nothing

            _  -> Just (minimumBy
                    (comparing (\mob ->
                        distanceSquared playerPosition (mobPosition mob)))
                    aliveMobs)

fireWeapon :: World -> World

fireWeapon world =
    case findNearestMob world of
        Nothing -> world

        Just target ->
            let
                currentPlayer = player world
                weapon = equippedWeapon currentPlayer

                (px, py) = position currentPlayer
                (mx, my) = mobPosition target

                dx = mx - px
                dy = my - py

                distance = sqrt (dx * dx + dy * dy)

                speed = projectileSpeed weapon
                range = attackRange weapon

                newProjectile = Projectile
                    { projectilePosition = (px, py)
                    , projectileVelocity =
                        (dx / distance * speed, dy / distance * speed)
                    , projectileDamage = weaponDamage weapon
                    , projectileRange = range
                    }

            in
                if distance == 0 || distance > range
                then world
                else world
                    { projectiles = newProjectile : projectiles world }

-- collison detection between projectiles and mobs
moveProjectiles :: World -> World

moveProjectiles world =
    world
        { projectiles = map moveOne (projectiles world) }

moveOne :: Projectile -> Projectile

moveOne projectile =
    let
        (x, y) = projectilePosition projectile
        (vx, vy) = projectileVelocity projectile

        distanceTravelled = sqrt (vx * vx + vy * vy)

    in
        projectile
            { projectilePosition = (x + vx, y + vy)
            , projectileRange = projectileRange projectile - distanceTravelled
            }


-- when the projectile hits the mob.

hasHit :: Projectile -> Mob -> Bool

hasHit projectile mob =
    distanceSquared
        (projectilePosition projectile)
        (mobPosition mob) <= 20^2