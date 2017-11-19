var fs = require('fs');
var casper = require('casper').create({
    // other options here
    viewportSize: {
        width: 3000,
        height: 4000
    }
}); 

casper.start('https://www.rotowire.com/daily/nba/defense-vspos.php?site=DraftKings&statview=last5&pos=PG', function() {
    this.echo('First Page: ' + this.getTitle());

	 var data = this.page.content;
	 fs.write('PG.html', data, 'w');  

});



casper.thenOpen('https://www.rotowire.com/daily/nba/defense-vspos.php?site=DraftKings&statview=last5&pos=SG', function() {
    this.echo('First Page: ' + this.getTitle());

	 var data = this.page.content;
	 fs.write('SG.html', data, 'w');  

});



casper.thenOpen('https://www.rotowire.com/daily/nba/defense-vspos.php?site=DraftKings&statview=last5&pos=PF', function() {
    this.echo('First Page: ' + this.getTitle());

	 var data = this.page.content;
	 fs.write('PF.html', data, 'w');  

});



casper.thenOpen('https://www.rotowire.com/daily/nba/defense-vspos.php?site=DraftKings&statview=last5&pos=SF', function() {
    this.echo('First Page: ' + this.getTitle());

	 var data = this.page.content;
	 fs.write('SF.html', data, 'w');  

});



casper.thenOpen('https://www.rotowire.com/daily/nba/defense-vspos.php?site=DraftKings&statview=last5&pos=C', function() {
    this.echo('First Page: ' + this.getTitle());

	 var data = this.page.content;
	 fs.write('C.html', data, 'w');  

});

casper.thenOpen('http://www.rotowire.com/basketball/nba_lineups.htm', function() {
    this.echo('First Page: ' + this.getTitle());

	 var data = this.page.content;
	 fs.write('Position.html', data, 'w');  

});



casper.run();