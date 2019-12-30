<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="/">
     <P>
          <xsl:value-of select="id('intro')"/>
     </P>
     <P>
          <xsl:value-of select="id('body')/text"/>
     </P>
     <P>
          <xsl:value-of select="id('text1')"/>
     </P>
</xsl:template>


</xsl:stylesheet>