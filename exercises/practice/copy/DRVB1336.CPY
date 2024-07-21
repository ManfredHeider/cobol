      *****************************************************************         
      *    COPY-MEMBER: DRVB1336                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FUER DAS LIST-OZP ZXVB0146                   *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: KLEEMEIER                                        *         
      *    DATUM:    29.03.2022                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-4457 !29.03.22! NEU                           ! KLEEMEIER *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      *                                                               *         
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *         
      *    FUNKTION DRVB1336 - ERMITTELN KONZ-SÄTZE FÜR VB-NUMMER     *         
      *                                                               *         
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *         
      *            DEM ERR-ERROR-BEREICH UND                          *         
      *            DEM STANDARD-STATUS-BEREICH                        *         
      *            (JEWEILS SEPARATE COPYS)                           *         
      *                                                               *         
      *****************************************************************         
                                                                                
       01 PI-DRVB1336.                                                          
          05 PI-DRVB1336-UFKT-CODE             PIC  9(002).                     
          05 PI-DRVB1336-MA-ST-NR              PIC S9(009)      BINARY.         
          05 PI-DRVB1336-KONZ-ERF-DAT          PIC  X(010).                     
                                                                                
       01 PO-DRVB1336.                                                          
          05 PO-DRVB1336-RC                    PIC  X(003).                     
             88 PO-DRVB1336-RC-OK                         VALUE 'OK '.          
             88 PO-DRVB1336-RC-EOF                        VALUE 'EOF'.          
             88 PO-DRVB1336-RC-NOT-FOUND                  VALUE 'NFD'.          
          05 PO-DRVB1336-DATEN.                                                 
             10 PO-DRVB1336-KONZ-SATZ          PIC  X(160).                     
      *---------------------------------------------------------------*         
      *----   ENDE  COPY-MEMBER  DRVB1336   --------------------------*         
      *---------------------------------------------------------------*         
