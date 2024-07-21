      *****************************************************************
      *    COPY-MEMBER: DRKI3266                                      *
      *    --------------------------------------------------------   *
      *    Cursor Full SELECT ZXKI0488 G79_KUNDEVERTRAG               *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !      AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01565!13.02.18!      Neuerstellung             ! tfl      *
      *DBO-10227!05.08.22!Erweiterung TT G79_KUNDEVERTRAG !u131774   *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3266
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3266.
           05 PI-DRKI3266-UFKT-CODE            PIC  9(002).
              88 PI-DRKI3266-UFKT-FIRST             VALUE 1.
              88 PI-DRKI3266-UFKT-NEXT              VALUE 2.
              88 PI-DRKI3266-UFKT-QUIT              VALUE 3.
 
           05  PI-DRKI3266-KEY.
              10 PI-DRKI3266-G79MANDANT        PIC S9(9) 
              COMP.
 
      *
       01  PO-DRKI3266.
           05 PO-DRKI3266-RC                   PIC  X(003).
              88 PO-DRKI3266-RC-OK                  VALUE 'OK '.
              88 PO-DRKI3266-RC-LEER                VALUE 'LER'.
              88 PO-DRKI3266-RC-EOF                 VALUE 'EOF'.
      *
           05 PO-DRKI3266-KEY.
              10 PO-DRKI3266-G79VERTRAGSNR         PIC  X(0015). 
              10 PO-DRKI3266-G79SPARRATEID         PIC S9(9) COMP.  
           05 PO-DRKI3266-DATEN.
              10 PO-DRKI3266-G79MANDANT            PIC S9(9) COMP.
              10 PO-DRKI3266-G79ISIN               PIC  X(0012).
              10 PO-DRKI3266-G79STATUS             PIC  X(0001).
              10 PO-DRKI3266-G79VRTRGBEGINN        PIC  X(0008).
              10 PO-DRKI3266-G79VRTRGENDE          PIC  X(0008).        
              10 PO-DRKI3266-G79ABSCHLVBNR         PIC S9(9) COMP.      
              10 PO-DRKI3266-G79KUNDENDATEN.                            
                49 PO-DRKI3266-G79KUNDE-LEN  PIC S9(4) COMP.
                49 PO-DRKI3266-G79KUNDE-TEXT PIC  X(382).
              10 PO-DRKI3266-G79SPARRATE           PIC S9(7)V9(2) 
                                                                 COMP-3.
              10 PO-DRKI3266-G79ZAHLUNGSART        PIC  X(0001).
              10 PO-DRKI3266-G79ABBUCHTERMIN       PIC  X(0001).
              10 PO-DRKI3266-G79IBAN               PIC  X(0034).
              10 PO-DRKI3266-G79BIC                PIC  X(0011).
              10 PO-DRKI3266-G79ABWNAME            PIC  X(0040).
              10 PO-DRKI3266-G79ENDEAUSDAT         PIC  X(0008).
              10 PO-DRKI3266-G79RATESCHLDAT        PIC  X(0008).        
              10 PO-DRKI3266-G79BESTANDGRAMM       PIC S9(7)V9(4)    
                                                                 COMP-3.
              10 PO-DRKI3266-G79BESTANDEURO        PIC S9(7)V9(2)    
                                                                 COMP-3.
              10 PO-DRKI3266-G79BUCHUNGSDAT        PIC  X(0008).
              10 PO-DRKI3266-G79RUECKGRAMM         PIC S9(4)V9(2) 
                                                                 COMP-3.
              10 PO-DRKI3266-G79SUMMEEIN           PIC S9(7)V9(2) 
                                                                 COMP-3.
              10 PO-DRKI3266-G79SUMMEAUS           PIC S9(7)V9(2) 
                                                                 COMP-3.
              10 PO-DRKI3266-G79RABATT             PIC  X(0001).
              10 PO-DRKI3266-G79KNDRABATT          PIC  X(0001).        
              10 PO-DRKI3266-G79KUNDENIDEXT        PIC S9(9) COMP.
              10 PO-DRKI3266-G79FLATRATE           PIC  X(0001).        
              10 PO-DRKI3266-G79RATEOEFFDAT        PIC  X(0008).        
              10 PO-DRKI3266-G79DYNAMIK            PIC S9(3)V9(2)
                                                                 COMP-3.
              10 PO-DRKI3266-G79SPARRATELFDNR      PIC S9(4) COMP.
              10 PO-DRKI3266-G79KUMAUSLIEF1        PIC S9(9) COMP.      
              10 PO-DRKI3266-G79KUMAUSLIEF2        PIC S9(9) COMP.      
              10 PO-DRKI3266-G79KUMAUSLIEF3        PIC S9(9) COMP.      
      *
      ******* ENDE COPY-MEMBER ***************************************