---
name: "Bug Report"
about: "Signaler un bug rencontré dans l'application"
title: "Bug : [Titre du problème]"
labels: ["bug"]
assignees: []
---

### Description du bug

- **Que se passe-t-il au moment de l'apparition du bug ?**

---

## Gravité et priorité

- **Importance :**

  - [ ] Critique (bloque l’usage complet)
  - [ ] Majeur (fonctionnalité importante impactée)
  - [ ] Mineur (bug  peu gênant)
  
- **Priorité de correction :**

  - [ ] Urgente
  - [ ] Normale
  - [ ] Basse

### Caractérisation du bug

- **Fichier(s) et/ou Fonction(s) dans laquelle vous avez trouvez le bug**  

---


### Comment reproduire le bug

- **Fréquence d'apparition du bug (systématiquement, de temps en temps) :** 

- **Est-ce un bug reproductible ou qui arrive de maniere aléatoire (erreur sur optim, erreur de convergence, seed spéciale):**  

- **L'erreur telle qu'elle apparait dans le log ou la console:**  

- **Comment le reproduire :**  
  1. Action 1
  2. Action 2
  3. ...

---

### Comportement attendu

- **Que doit-il se passer en temps normal?**

---

## Historique

- Changement récent ou PR qui pourrait l’avoir introduit ?

---

### Informations sur l'environnement d'apparition du bug

- Système d'exploitation (local et machine virtuelle) :
- Version de R utilisée : 
- Version du package : 
- Environnement d'exécution : Rstudio, shell, positron
- Branche Git  (si ce n’est pas main) :
- options : Liste et version des dépendances (output de _sessionInfo()_)


---

### Informations supplémentaires

Si nécessaire  
* Capture d'ecran, logs, données ou tout autre détail pertinant.
---

## Tests 
- Tests unitaires existants couvre le bug? :
  - [ ] Oui
  - [ ] Non
- Si oui, pourquoi il ne l'a pas détecté?

---

## Organisation

@ahboualam @MechantRouquin

- **À discuter avant d’ouvrir une branche/PR :**
    
  - [ ] Découpage en sous-issues nécessaire ?  
  - [ ] Planification nécessaire pour résoudre l’issue ?  
  - [ ] Formation ou ressources nécessaires pour résoudre l’issue ?  
  - [ ] Modifications de code requises :
    - Nouvelles fonctions à créer : signatures et objectifs
    - Fonctions existantes à modifier : préciser les changements
  - [ ] Tests supplémentaires à ajouter pour valider la correction ?
  