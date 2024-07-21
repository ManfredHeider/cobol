      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.11.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3109.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3109.CPv  $
      *
      *   Rev 2.2   Dec 19 2016 13:30:42   t13ac20
      *AKI01466: Multirow OZP
      *
      *   Rev 2.1   Dec 07 2016 10:57:56   t13ac20
      *AKI01466: Multirow OZP
      *
      *   Rev 2.0   Nov 11 2016 11:54:49 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3109                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3109                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         11.11.2016 / 11:54:49 / T13AC20
      *      MIT MANUELLEN ÄNDERUNGEN:
      *         - FUNKTION DRKI3109 SORTIERT NACH G51PLFDNR
      *         - JOIN AUF VERTRAG ENTFERNT INKL. ANPASSUNG VON
      *           PI-DRKI3109-KEY
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01466!21.09.16!2.1 ! NEUERSTELLUNG             ! SWO      *
      * AKI01466!19.12.16!2.2 ! FELDER ENTFERNT:          ! SWO      *
      *         !        !    !   - G51PABRECHDAT         !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3109
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3109.
          05 PI-DRKI3109-PROGRAMM                 PIC  X(008).
          05 PI-DRKI3109-UFKT-CODE                PIC  9(002).
 
          05  PI-DRKI3109-KEY.
            10 PI-DRKI3109-G51PVERTRAGSNR         PIC  X(015).
 
      *
       01  PO-DRKI3109.
           05  PO-DRKI3109-RC                     PIC  X(003).
             88 PO-DRKI3109-RC-OK                        VALUE 'OK '.
             88 PO-DRKI3109-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI3109-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI3109-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI3109-DATEN.
 
           10 PO-DRKI3109-G51PVERTRAGSNR PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI3109-G51PLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3109-G51PSTATUS PIC  X(0001)
                                   OCCURS 100.
           10 PO-DRKI3109-G51PVERSART PIC  X(0001)
                                   OCCURS 100.
           10 PO-DRKI3109-G51PVRTRGBEGINN PIC  X(0008)
                                   OCCURS 100.
           10 PO-DRKI3109-G51PVRTRGENDE PIC  X(0008)
                                   OCCURS 100.
           10 PO-DRKI3109-G51PNACHNAME PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI3109-G51PVORNAME PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI3109-G51PGEBDAT PIC  X(0008)
                                   OCCURS 100.
           10 PO-DRKI3109-G51PANREDE PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3109-G51PGESCHLECHT PIC  X(0001)
                                   OCCURS 100.
           10 PO-DRKI3109-G51PABRECHKZ PIC  X(0001)
                                   OCCURS 100.
           10 PO-DRKI3109-G51PZUSRAHMEN PIC  X(0001)
                                   OCCURS 100.
 
      *
      ******* ENDE COPY-MEMBER ***************************************
 

