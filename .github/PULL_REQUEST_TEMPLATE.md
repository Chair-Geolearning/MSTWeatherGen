## Type

- [ ] Bug(s)
- [ ] Feature(s)
- [ ] Autre

## Description

Merci de décrire brièvement ce qui a été fait dans cette Pull Request. Expliquez le but principal cette PR.  
Si la PR est liée à une issue, mentionnez-la (ex: "Closes #123").

Cette PR introduit-elle des changements cassants ? Si oui, les décrire.


---

## Changement effectué

- [ ] Correction de bug(s)
- [ ] Nouvelle fonctionnalité
- [ ] Amélioration du code
- [ ] Mise à jour de la documentation
- [ ] Ajout/modification de tests unitaires
- [ ] Mise à jour des métadonnées (DESCRIPTION, NAMESPACE, etc.)
- [ ] Autre (préciser) :  


- **Lister les fichiers et principales fonctions impactées :**  


- **Impact sur le comportement existant :**
  - Cette PR introduit-elle des changements cassants (breaking changes) ? Si oui, les décrire.

---

## Vérification

- [ ] J’ai testé manuellement les modifications (OS :  - Version de R :).
- [ ] J’ai exécuté les tests unitaires locaux (`devtools::test()`) et ils passent.
- [ ] J’ai vérifié que le package se build et et check correctement (`devtools::build()` et `devtools::check()`).
- [ ] J’ai vérifié que le package se build et et s’installe correctement (`devtools::install()`).
- [ ] J’ai vérifié que le CI/CD passe (lien vers les résultats : [ex: GitHub CI](#)).
- [ ] J’ai mis à jour la documentation (vignettes, help, README) si nécessaire ((`devtools::document()`)).
- [ ] J'ai vérifié que les conventions de style sont respectées (lintr / styleR).

---


## Notes supplémentaires


---

## À discuter avant la fusion

- [ ] De nouvelles librairies ou dépendances ajoutées ?  
  - Si oui, mentionner les librairies et justifier leur ajout.

- [ ] A-t-on modifié le code original ?  
  - Si oui, spécifier les fonctions et le fichier concerné.

- [ ] Impact sur les utilisateurs :  
  - Cette PR nécessite-t-elle une communication spécifique (ex: note de version) ?

- [ ] Compatibilité :  
  - Les modifications sont-elles rétrocompatibles ?

- [ ] Performance :  
  - Y a-t-il un impact sur les performances ? Si oui, le décrire.
---

### Instructions pour la revue

- **Points spécifiques à vérifier :**  
  - Exemple : "Vérifier que la nouvelle fonction `bar()` gère correctement les entrées `NA`."

- **Comment tester cette PR ?**
  ```r
  # Exemple de code pour tester les modifications
  library(MSTWheaterGen)
  ```

---

### Checklist avant de fusionner la PR :

- [ ] Code testé et validé.
- [ ] Documentation mise à jour (si nécessaire).
- [ ] Aucune erreur de compilation ou de tests.
- [ ] Aucun test échoue (locaux et CI/CD)
- [ ] Le code suit les conventions du projet (style, naming)
- [ ] Les dépendances sont minimales et justifiées
- [ ] Le comportement correspond à la description
- [ ] Les conflits avec la branche main sont résolus
