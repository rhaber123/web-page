<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="car[not(@checked)]">
     <P>
          <B style="color:red">
               <xsl:value-of select="@id"/>
          </B>
     </P>
</xsl:template>

<xsl:template match="car[@checked]">
     <P>
          <B style="color:blue">
               <xsl:value-of select="@id"/>
          </B>
     </P>
</xsl:template>


</xsl:stylesheet>