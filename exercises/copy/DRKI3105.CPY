      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.09.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3105.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3105.CPv  $
      *
      *   Rev 2.2   Dec 19 2016 13:16:46   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.1   Dec 07 2016 10:52:00   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.0   Sep 21 2016 13:35:14 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3105                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3105                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         19.12.2016 / 13:11:06 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI3105
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3105.
           05  PI-DRKI3105-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3105-G51PLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3105-G51PVERTRAGSNR   PIC  X(0015).
END   *
 
           05  PI-DRKI3105-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                         PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3105-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3105-G51PABRECHKZ   PIC  X(0001).
             10 PI-DRKI3105-G51PANREDE   PIC S9(4) COMP.
             10 PI-DRKI3105-G51PGEBDAT   PIC  X(0008).
             10 PI-DRKI3105-G51PGESCHLECHT   PIC  X(0001).
             10 PI-DRKI3105-G51PNACHNAME   PIC  X(0030).
             10 PI-DRKI3105-G51PSTATUS   PIC  X(0001).
             10 PI-DRKI3105-G51PVERSART   PIC  X(0001).
             10 PI-DRKI3105-G51PVORNAME   PIC  X(0030).
             10 PI-DRKI3105-G51PVRTRGBEGINN   PIC  X(0008).
             10 PI-DRKI3105-G51PVRTRGENDE   PIC  X(0008).
             10 PI-DRKI3105-G51PZUSRAHMEN   PIC  X(0001).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3105-NI1.
           05 PI-DRKI3105-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI3105-NI7.
           05 PI-DRKI3105-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.
 
      *
       01  PO-DRKI3105.
           05  PO-DRKI3105-RC                     PIC  X(003).
               88 PO-DRKI3105-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3105-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

