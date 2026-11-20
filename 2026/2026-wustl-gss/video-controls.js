// Shared controls align playback positions when starting the comparison.
// Native video controls remain available for inspecting individual results.
document.addEventListener('DOMContentLoaded', () => {
  const allVideos = [...document.querySelectorAll('.video-comparison video')];
  const pauseAll = list => list.forEach(video => video.pause());
  document.querySelectorAll('[data-video-action]').forEach(button => {
    button.addEventListener('click', async () => {
      const slide = button.closest('section') || document;
      const videos = [...slide.querySelectorAll('.video-comparison video')];
      const status = slide.querySelector('.video-status');
      const action = button.dataset.videoAction;
      if (action === 'pause') { pauseAll(videos); return; }
      const time = action === 'restart' ? 0 : (videos[0]?.currentTime ?? 0);
      pauseAll(videos);
      videos.forEach(video => { video.currentTime = time; });
      const results = await Promise.allSettled(videos.map(video => video.play()));
      if (results.some(result => result.status === 'rejected')) {
        pauseAll(videos);
        if (status) status.textContent = 'Playback failed. Try the individual video controls.';
      } else if (status) { status.textContent = ''; }
    });
  });
  // Pause on leaving any comparison slide; no video starts automatically.
  if (typeof Reveal !== 'undefined') {
    Reveal.on('slidechanged', event => {
      pauseAll(allVideos.filter(video => !event.currentSlide.contains(video)));
    });
  }
});
