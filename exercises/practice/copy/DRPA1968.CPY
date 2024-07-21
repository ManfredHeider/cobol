      *****************************************************************
      *    COPY-MEMBER: DRPA1968                                     
      *                                                              
      *    FUNKTION KUMMULIEREN ZU PMPA0145                          
      *                                                              
      *    DIESE FUNKTION WIRD AUSSCHLIESSLICH IM PMPA0145 ZUR       
      *    KUMMULIERUNG DER BEWEGUNGEN GENUTZT. HIERIN IST KEINE     
      *    NORMIERUNG DES PPFL-BETRAGS ENTHALTEN.                    
      *                                                              
      *                                                              
      *                                                              
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DSY04360   !08.02.22! NEU                          ! SCHEIDEL *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1968                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1968.
          05 PI-DRPA1968-PZVP-VERSION               PIC S9(004) BINARY.
          05 PI-DRPA1968-DATEN.
             10 PI-DRPA1968-PMPE-GSMT-PROV-DAT      PIC  X(010).
             10 PI-DRPA1968-PMPP.
                COPY DRPA0290.
      *
       01 PO-DRPA1968.
          05 PO-DRPA1968-RC                         PIC  X(003).
             88 PO-DRPA1968-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1968-RC-VSTAT-GEFUELLT             VALUE 'VSG'.
          05 PO-DRPA1968-DATEN.
             10 PO-DRPA1968-PMPP.
                COPY DRPA0290.
      *
      ******* ENDE COPY-MEMBER **************************************
