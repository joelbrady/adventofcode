package twentyeighteen2.twentyfour

import assertThat

fun main() {
    tests()
}

enum class DamageType {
    FIRE,
    SLASHING,
    BLUDGEONING,
    COLD,
    RADIATION
}

data class Group(
    val damageType: DamageType,
    val size: Int,
    val attackDamage: Int,
    val hitPoints: Int,
    val weaknesses: Set<DamageType>,
    val immunities: Set<DamageType>,
    val initiative: Int
) {
    fun damage(attack: Attack): Int = when (attack.type) {
        in immunities -> 0
        in weaknesses -> attack.magnitude * 2
        else -> attack.magnitude
    }

    fun attack(): Attack = Attack(magnitude = size * attackDamage, type = damageType)
}

data class Attack(val magnitude: Int, val type: DamageType)

/*
    Immune System:
    17 units each with 5390 hit points (weak to radiation, bludgeoning) with
     an attack that does 4507 fire damage at initiative 2
    989 units each with 1274 hit points (immune to fire; weak to bludgeoning,
     slashing) with an attack that does 25 slashing damage at initiative 3

    Infection:
    801 units each with 4706 hit points (weak to radiation) with an attack
     that does 116 bludgeoning damage at initiative 1
    4485 units each with 2961 hit points (immune to radiation; weak to fire,
     cold) with an attack that does 12 slashing damage at initiative 4
 */

val immune1: Group = Group(
    damageType = DamageType.FIRE,
    size = 17,
    attackDamage = 4507,
    hitPoints = 5390,
    weaknesses = setOf(DamageType.RADIATION, DamageType.BLUDGEONING),
    immunities = emptySet(),
    initiative = 2
)

val immune2: Group = Group(
    damageType = DamageType.SLASHING,
    size = 989,
    attackDamage = 25,
    hitPoints = 1274,
    weaknesses = setOf(DamageType.SLASHING, DamageType.BLUDGEONING),
    immunities = setOf(DamageType.FIRE),
    initiative = 3
)

val infection1: Group = Group(
    damageType = DamageType.BLUDGEONING,
    size = 801,
    attackDamage = 116,
    hitPoints = 4706,
    weaknesses = setOf(DamageType.RADIATION),
    immunities = emptySet(),
    initiative = 1
)

val infection2: Group = Group(
    damageType = DamageType.SLASHING,
    size = 4485,
    attackDamage = 12,
    hitPoints = 2961,
    weaknesses = setOf(DamageType.FIRE, DamageType.COLD),
    immunities = setOf(DamageType.RADIATION),
    initiative = 3
)

fun tests() {
    example1Damage()
}

fun example1Damage() {
    /*
        Immune System:
        Group 1 contains 17 units
        Group 2 contains 989 units
        Infection:
        Group 1 contains 801 units
        Group 2 contains 4485 units

        Infection group 1 would deal defending group 1 185832 damage
        Infection group 1 would deal defending group 2 185832 damage
        Infection group 2 would deal defending group 2 107640 damage
        Immune System group 1 would deal defending group 1 76619 damage
        Immune System group 1 would deal defending group 2 153238 damage
        Immune System group 2 would deal defending group 1 24725 damage
     */

    assertThat(immune1.size).isEqualTo(17)
    assertThat(immune2.size).isEqualTo(989)
    assertThat(infection1.size).isEqualTo(801)
    assertThat(infection2.size).isEqualTo(4485)

    assertThat(immune1.damage(infection1.attack())).isEqualTo(185832)
    assertThat(immune2.damage(infection1.attack())).isEqualTo(185832)
    assertThat(immune2.damage(infection2.attack())).isEqualTo(107640)

    assertThat(infection1.damage(immune1.attack())).isEqualTo(76619)
    assertThat(infection2.damage(immune1.attack())).isEqualTo(153238)
    assertThat(infection1.damage(immune2.attack())).isEqualTo(24725)
}
