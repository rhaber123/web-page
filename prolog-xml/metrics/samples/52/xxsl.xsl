<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="/">
     <P>
          <xsl:value-of select="//text"/>
     </P>
     <P>
          <xsl:value-of select="translate(//text,'egos','EGOS')"/>
     </P>
     <P>
          <xsl:value-of select="translate(//text,'se','d')"/>
     </P>
     <P>
          <xsl:value-of select="translate(//text,'gseo','bad')"/>
     </P>
     <P>
          <xsl:value-of select="translate(//text,'gseg','bksC')"/>
     </P>
</xsl:template>


</xsl:stylesheet>