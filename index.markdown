---
layout: home
title: Home
---

<div style="display:flex;gap:20px;">
 <script src="https://platform.linkedin.com/badges/js/profile.js" async defer type="text/javascript"></script>
 <div style="flex:1;">
  <img src="{{ '/assets/img/GradPhoto.jpeg' | relative_url }}" style="width:100%;border-radius:8px;">
  <img src="{{ '/assets/img/Photo1.jpeg' | relative_url }}" style="width:100%;margin-top:20px;border-radius:8px;">
  
 </div>

 <div style="flex:1;">
  <h2>Welcome to my Portfolio Website</h2>
  <p>I am a Recent Graduate of University of Rochester seeking to add value to a growing organization.</p>
  <p>This website is a portfolio of my past projects.</p>
  <h2>Connect with Me</h2>
  <div class="badge-base LI-profile-badge" data-locale="en_US" data-size="large" data-theme="light" data-type="HORIZONTAL" data-vanity="gdaher" data-version="v1"><a class="badge-base__link LI-simple-link" href="https://www.linkedin.com/in/gdaher?trk=profile-badge">George Daher</a></div>
  <h2><a href="{{ '/resume.html' | relative_url }}">My Resume</a></h2>
  <h2><a href="{{ '/projects.html' | relative_url }}">My Projects</a></h2>
  <p><button class="home-btn" onclick="location.href=\'{{ '/projects/project1.html' | relative_url }}\'">Project 1 – IMDB Ratings</button></p>
  <p><button class="home-btn" onclick="location.href=\'{{ '/projects/project2.html' | relative_url }}\'">Project 2 – MLB Lineups</button></p>
  <p><button class="home-btn" onclick="location.href=\'{{ '/projects/project3.html' | relative_url }}\'">Project 3 – DataFest 2023</button></p>
  <p><button class="home-btn" onclick="location.href=\'{{ '/projects/project4.html' | relative_url }}\'">Project 4 – Simulation Study</button></p>
  <p><button class="home-btn" onclick="location.href=\'{{ '/projects/project5.html' | relative_url }}\'">Project 5 – DataFest 2024</button></p>
  <p><button class="home-btn" onclick="location.href=\'{{ '/projects/project6.html' | relative_url }}\'">Project 6 – FEC Elections</button></p>
  <p><button class="home-btn" onclick="location.href=\'{{ '/projects/project7.html' | relative_url }}\'">Project 7 – PCA Decomposition</button></p>
 </div>
</div>
