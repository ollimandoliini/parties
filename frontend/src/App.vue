<script setup lang="ts">
import Party from './components/Party.vue'
</script>

<template>
  <Party party-name="Synttärit" party-description="Tervetuloa tervetuloa" />
  <router-view />
</template>

<style>
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  margin-top: 60px;
}
</style>
