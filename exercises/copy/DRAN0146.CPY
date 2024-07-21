      ****************************************************************
      *    COPY-MEMBER: DRAN0146                                     *
      *    FUNKTION   : ERMITTLUNG DER GEWERBEBERECHTIGUNG           *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: U. BOLL                                         *
      *    DATUM:    25.10.2006                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00697!25.10.06!2.1 !NEU                        !U. BOLL   *
      * AAN00982!25.11.10!2.2 !NEU                        !M.HEIDER  *
      * AAN01042!30.01.12!2.3 !34C ANLAGEBERATUNG HINZU   !RAHNER    *
      * AAN01089!09.11.12!2.4 !34F                        !HEIDER    *
      * AAN01127!05.07.13!2.6 !ZUSATZINFO BEI 34F         !RAHNER    *
      * AAN01258!30.05.16!2.7 !34F REG-NR                 !RAHNER    *
      * AAN01259!31.05.16!2.7 !34I                        !RAHNER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      *****************************************************************
      * LOGBUCH:
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME
      * --------------------------------------------------------------*
      * ANTRAG-1928!21.03.23! NEUE GES. 72 BERECHTIGUNG VB ! RAHNER
      *            !        !                              !
      *            !        !                              !
      *            !        !                              !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0146                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0146.
          05 PI-DRAN0146-DATEN.
             10 PI-DRAN0146-VB-NR                PIC S9(009) COMP.
      *
       01 PO-DRAN0146.
          05 PO-DRAN0146-RC                      PIC  X(003).
             88 PO-DRAN0146-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0146-RC-NOK                    VALUE 'NOK'.
             88 PO-DRAN0146-RC-DAT                    VALUE 'DAT'.
          05 PO-DRAN0146-DATEN.
             10 PO-DRAN0146-34C-GEWO             PIC  X(001).
             10 PO-DRAN0146-AFO                  PIC  X(001).
             10 PO-DRAN0146-FG                   PIC  X(001).
             10 PO-DRAN0146-VA                   PIC  X(001).
             10 PO-DRAN0146-127-BESCHR           PIC  X(001).
             10 PO-DRAN0146-127-UNBESCHR         PIC  X(001).
      *         VERMÖGENSSCHADENHAFTPFLICHT      
             10 PO-DRAN0146-VSH                  PIC  X(001).
      *         WERTPAPIERVERMITTLER-GEWERBESCHEIN
             10 PO-DRAN0146-WG                   PIC  X(001).
      *         34C ANLAGEBERATUNG
             10 PO-DRAN0146-34C-ERR-RC           PIC  X(006).
                88 PO-DRAN0146-34C-ERR-KEINER         VALUE 'OK    '.
                88 PO-DRAN0146-34I-ERR-DAT-34C        VALUE '34C-ED'.
             10 PO-DRAN0146-34C-ANLAGEBERATUNG   PIC  X(001).
             10 PO-DRAN0146-34C-DATUM            PIC  X(010).
      *         34F
             10 PO-DRAN0146-34F-ERR-RC           PIC  X(006).
                88 PO-DRAN0146-34F-ERR-KEINER         VALUE 'OK    '.
                88 PO-DRAN0146-34F-ERR-DAT-34F        VALUE '34F-ED'.
                88 PO-DRAN0146-34F-ERR-REG-NR-34F     VALUE '34F-ER'.
             10 PO-DRAN0146-34F-GEWO             PIC  X(001).
             10 PO-DRAN0146-34F-ZUSATZ-INFO      PIC  X(001).
      *         34I
             10 PO-DRAN0146-34I-ERR-RC           PIC  X(006).
                88 PO-DRAN0146-34I-ERR-KEINER         VALUE 'OK    '.
                88 PO-DRAN0146-34I-ERR-DAT-34I        VALUE '34I-ED'.
                88 PO-DRAN0146-34I-ERR-REG-NR-34I     VALUE '34I-ER'.
             10 PO-DRAN0146-34I-ANLAGEBERATUNG   PIC  X(001).
      *         GEWERBESCHEIN "VERMÖGENSBERATER" (VB)
             10 PO-DRAN0146-VB                   PIC  X(001).
             
      ******** ENDE COPY-MEMBER **************************************


