      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.09.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3106.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3106.CPv  $
      *
      *   Rev 2.2   Dec 19 2016 13:16:56   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.1   Dec 07 2016 10:52:10   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.0   Sep 21 2016 13:35:16 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3106                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3106                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         19.12.2016 / 13:11:08 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI3106
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3106.
           05  PI-DRKI3106-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3106-G51PLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3106-G51PVERTRAGSNR   PIC  X(0015).
END   *
 
           05  PI-DRKI3106-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                         PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3106-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3106-G51PABRECHKZ   PIC  X(0001).
             10 PI-DRKI3106-G51PANREDE   PIC S9(4) COMP.
             10 PI-DRKI3106-G51PGEBDAT   PIC  X(0008).
             10 PI-DRKI3106-G51PGESCHLECHT   PIC  X(0001).
             10 PI-DRKI3106-G51PNACHNAME   PIC  X(0030).
             10 PI-DRKI3106-G51PSTATUS   PIC  X(0001).
             10 PI-DRKI3106-G51PVERSART   PIC  X(0001).
             10 PI-DRKI3106-G51PVORNAME   PIC  X(0030).
             10 PI-DRKI3106-G51PVRTRGBEGINN   PIC  X(0008).
             10 PI-DRKI3106-G51PVRTRGENDE   PIC  X(0008).
             10 PI-DRKI3106-G51PZUSRAHMEN   PIC  X(0001).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3106-NI1.
           05 PI-DRKI3106-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3106-NI7.
           05 PI-DRKI3106-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3106.
           05  PO-DRKI3106-RC                     PIC  X(003).
               88 PO-DRKI3106-RC-OK               VALUE 'OK '.
               88 PO-DRKI3106-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

