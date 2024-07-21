      ****************************************************************          
      * AENDERUNGEN:                                                 *          
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *          
      * -------------------------------------------------------------*          
      *AKI000001!12.08.98! 01 ! NEUERSTELLUNG             ! HUEBNER  *          
      *         !        !    !                           !          *          
      *         !        !    !                           !          *          
      *                                                              *          
      * ENDE-LOGBUCH                                                 *          
      ****************************************************************          
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *          
      *    FÜR DIE FUNKTION DRKI0563                                 *          
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *          
      *            DEM ERR-ERROR-BEREICH UND                         *          
      *            DEM STANDARD-STATUS-BEREICH                       *          
      *            (JEWEILS SEPARATE COPYS)                          *          
      *                                                              *          
      ****************************************************************          
       01 PI-DRKI0563.                                                          
          05 PI-DRKI0563-PZVP-VERSION             PIC S9(004) BINARY.           
          05 PI-DRKI0563-PROGRAMM                 PIC  X(008).                  
          05 PI-DRKI0563-UFKT                     PIC  9(002).                  
          05 PI-DRKI0563-SCHALTER                 PIC  9.                       
          05 PI-DRKI0563-DATEN.                                                 
           07  PI-DRKI0563-ID.                                                  
               10  PI-DRKI0563-IDENTIFIER  PIC 9(08).                           
               10  PI-DRKI0563-IDENTIFIER-RED REDEFINES                         
                                           PI-DRKI0563-IDENTIFIER.              
                   15  PI-DRKI0563-GES     PIC 9(4).                            
                   15  PI-DRKI0563-FILL    PIC 9(4).                            
      *                                                                         
       01 PO-DRKI0563.                                                          
          05 PO-DRKI0563-RC                       PIC  X(003).                  
             88 PO-DRKI0563-RC-OK                        VALUE 'OK '.           
             88 PO-DRKI0563-RC-NOK                       VALUE 'NOK'.           
          05 PO-DRKI0563-DATEN.                                                 
           07  PO-DRKI0563-OUTID           PIC X(13).                           
           07  PO-DRKI0563-TIMESTAMP       PIC X(26).                           
      *                                                                         
      ***** ENDE-COPY-MEMBER *****************************************          
