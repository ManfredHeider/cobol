      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.09.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3093.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3093.CPv  $
      *
      *   Rev 2.2   Dec 19 2016 13:22:44   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.1   Dec 07 2016 10:49:50   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.0   Sep 20 2016 14:14:23 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3093                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3093                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         19.12.2016 / 13:17:36 / T13AC20
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01466!21.09.16!2.1 ! NEUERSTELLUNG             ! SWO      *
      * AKI01466!19.12.16!2.2 ! FELDER ENTFERNT:          ! SWO      *
      *         !        !    !   - KVPABRECHDAT          !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3093
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3093.
           05  PI-DRKI3093-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3093-KVPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3093-KVPVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI3093-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI3093-NI-KVPABRECHKZ     PIC S9(4) COMP.
             15 PI-DRKI3093-NI-KVPBEGINNDATUM  PIC S9(4) COMP.
             15 PI-DRKI3093-NI-KVPENDEDATUM    PIC S9(4) COMP.
             15 PI-DRKI3093-NI-KVPGEBDATUM     PIC S9(4) COMP.
             15 PI-DRKI3093-NI-KVPNACHNAME     PIC S9(4) COMP.
             15 PI-DRKI3093-NI-KVPVERSART      PIC S9(4) COMP.
             15 PI-DRKI3093-NI-KVPVORNAME      PIC S9(4) COMP.
             15 PI-DRKI3093-NI-KVPVORVENDDATUM PIC S9(4) COMP.
             15 PI-DRKI3093-NI-KVPVORVGES-KNR  PIC S9(4) COMP.
             15 PI-DRKI3093-NI-KVPVORVGES-KTY  PIC S9(4) COMP.
             15 PI-DRKI3093-NI-KVPVORVKUENDKZ  PIC S9(4) COMP.
             15 PI-DRKI3093-NI-KVPZUSRAHMEN    PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3093-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3093-KVPABRECHKZ   PIC  X(0001).
             10 PI-DRKI3093-KVPANREDE-KNR   PIC S9(4) COMP.
             10 PI-DRKI3093-KVPANREDE-KTY   PIC S9(4) COMP.
             10 PI-DRKI3093-KVPBEGINNDATUM   PIC  X(010).
             10 PI-DRKI3093-KVPENDEDATUM   PIC  X(010).
             10 PI-DRKI3093-KVPGEBDATUM   PIC  X(010).
             10 PI-DRKI3093-KVPNACHNAME   PIC  X(0030).
             10 PI-DRKI3093-KVPPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI3093-KVPVERSART   PIC  X(0001).
             10 PI-DRKI3093-KVPVORNAME   PIC  X(0030).
             10 PI-DRKI3093-KVPVORVENDDATUM   PIC  X(010).
             10 PI-DRKI3093-KVPVORVGES-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI3093-KVPVORVGES-KTY   PIC S9(4) COMP.
             10 PI-DRKI3093-KVPVORVKUENDKZ   PIC  X(0001).
             10 PI-DRKI3093-KVPZUSRAHMEN   PIC  X(0001).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3093-NI1.
           05 PI-DRKI3093-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 12.
       01  PI-DRKI3093-NI7.
           05 PI-DRKI3093-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 12.
 
      *
       01  PO-DRKI3093.
           05  PO-DRKI3093-RC                     PIC  X(003).
               88 PO-DRKI3093-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3093-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

