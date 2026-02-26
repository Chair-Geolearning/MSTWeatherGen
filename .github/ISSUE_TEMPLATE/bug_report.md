---
name: "Bug Report"
about: "Signaler un bug rencontré dans l'application"
title: "Bug : [Titre du problème]"
labels: ["bug"]
assignees: []
---

### Comportement lors du bug

- **Que se passe-t-il au moment de l'apparition du bug ?**

---

### Caractérisation du bug

- **Fichier et/ou Fonction dans laquelle vous avez trouvez le bug**  

---

### Comment reproduire le bug

- **Fréquence d'apparition du bug (systématiquement, de temps en temps) :**  

- **L'erreur telle qu'elle apparait dans le log ou la console:**  

- **Comment le reproduire :**  
  1. Action 1
  2. Action 2
  3. ...

---

### Comportement attendu

- **Que doit-il se passer en temps normal?**

---


---

### Informations sur l'environnement d'apparition du bug

- Système d'exploitation (local et machine virtuelle) :
- Version de R utiliséé : 
- Version du package : 
- Environnement d'exécution : Rstudio, shell, positron
- options : Liste et version des dépendances (output de _sessionInfo()_)


---

### Informations supplémentaires

Si nécessaire  
* Capture d'ecran, logs, données ou tout autre détail pertinant.
---

## Partie réservée aux développeurs

@ahboualam @MechantRouquin

- **À discuter avant d’ouvrir une branche/PR :**

  - Est-ce qu’il y a un besoin de découper l’issue en sous-issues ?  
    - Si oui, ouvrir d’autres issues.
  
  - Est-ce qu’il y a besoin de planifier la résolution de l’issue?  
    - Si oui, faire un planning détaillé.
  
  - Est-ce qu’il y a besoin de se former pour résoudre l’issue ?  
    - Si oui, trouver des ressources pédagogiques adaptées.
  
  - Est-ce qu’il y a besoin d’écrire de nouvelles fonctions ou d’en modifier certaines par rapport au code original?  
    - Si oui, écrire dès maintenant la signature de la nouvelle fonction ou les modifications apportées à une signature existante.
  
  - Est-ce qu’il est possible d’écrire du code permettant de reproduire le bug ?   
  - Y a-t-il des testunitires qui couvrent déjà cette partie du code ?  
    - si oui, lesquels ?  
  - Besoin de test supplémentaires ?  
    - Faut-il ajouter des tests pour valider la correction ?  
  