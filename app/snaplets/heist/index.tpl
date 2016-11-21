<apply template="_base">
  <ifLoggedIn>
    <ignore>
    <apply template="mockup"/>
    </ignore>

    <!-- JavaScripts Deps -->
    <script src="/static/app.js"></script>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>
</apply>
