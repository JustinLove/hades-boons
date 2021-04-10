Aphrodite = "AphroditeUpgrade"
Ares = "AresUpgrade"
Artemis = "ArtemisUpgrade"
Athena = "AthenaUpgrade"
Demeter = "DemeterUpgrade"
Dionysus = "DionysusUpgrade"
Poseidon = "PoseidonUpgrade"
Zeus = "ZeusUpgrade"

duos = [
  [Aphrodite, Ares],
  [Artemis, Aphrodite],
  [Aphrodite, Athena],
  [Aphrodite, Demeter],
  [Dionysus, Aphrodite],
  [Aphrodite, Poseidon],
  [Zeus, Aphrodite],
  [Artemis, Ares],
  [Ares, Athena],
  [Ares, Demeter],
  [Dionysus, Ares],
  [Poseidon, Ares],
  [Zeus, Ares],
  [Athena, Artemis],
  [Demeter, Artemis],
  [Dionysus, Artemis],
  [Poseidon, Artemis],
  [Artemis, Zeus],
  [Athena, Demeter],
  [Dionysus, Athena],
  [Poseidon, Athena],
  [Athena, Zeus],
  [Demeter, Dionysus],
  [Demeter, Poseidon],
  [Demeter, Zeus],
  [Dionysus, Poseidon],
  [Dionysus, Zeus],
  [Poseidon, Zeus],
]

def perms(seq, pool)
  #p [seq, pool.length]
  head = seq.first[0]
  tail = seq.last[1]
  candidates,noleft = pool.select {|a,b| a != head}.partition {|a,b| a == tail}
  #p 'can', candidates
  rest = noleft.select {|a,b| b != tail}
  candidates.each do |pair|
    perms(seq + [pair], rest.dup)
  end
  if candidates.length < 1
    if seq.length == 8
      p seq.map {|a,b| a}
    end
  end
end

perms([duos.shift], duos)
p '---------------------------------------'
