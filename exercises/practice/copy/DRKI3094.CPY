      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.09.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3094.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3094.CPv  $
      *
      *   Rev 2.2   Dec 19 2016 13:22:54   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.1   Dec 07 2016 10:50:00   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.0   Sep 20 2016 14:14:26 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3094                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3094                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         19.12.2016 / 13:17:38 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI3094
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3094.
           05  PI-DRKI3094-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3094-KVPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3094-KVPVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI3094-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI3094-NI-KVPABRECHKZ     PIC S9(4) COMP.
             15 PI-DRKI3094-NI-KVPBEGINNDATUM  PIC S9(4) COMP.
             15 PI-DRKI3094-NI-KVPENDEDATUM    PIC S9(4) COMP.
             15 PI-DRKI3094-NI-KVPGEBDATUM     PIC S9(4) COMP.
             15 PI-DRKI3094-NI-KVPNACHNAME     PIC S9(4) COMP.
             15 PI-DRKI3094-NI-KVPVERSART      PIC S9(4) COMP.
             15 PI-DRKI3094-NI-KVPVORNAME      PIC S9(4) COMP.
             15 PI-DRKI3094-NI-KVPVORVENDDATUM PIC S9(4) COMP.
             15 PI-DRKI3094-NI-KVPVORVGES-KNR  PIC S9(4) COMP.
             15 PI-DRKI3094-NI-KVPVORVGES-KTY  PIC S9(4) COMP.
             15 PI-DRKI3094-NI-KVPVORVKUENDKZ  PIC S9(4) COMP.
             15 PI-DRKI3094-NI-KVPZUSRAHMEN    PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3094-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3094-KVPABRECHKZ   PIC  X(0001).
             10 PI-DRKI3094-KVPANREDE-KNR   PIC S9(4) COMP.
             10 PI-DRKI3094-KVPANREDE-KTY   PIC S9(4) COMP.
             10 PI-DRKI3094-KVPBEGINNDATUM   PIC  X(010).
             10 PI-DRKI3094-KVPENDEDATUM   PIC  X(010).
             10 PI-DRKI3094-KVPGEBDATUM   PIC  X(010).
             10 PI-DRKI3094-KVPNACHNAME   PIC  X(0030).
             10 PI-DRKI3094-KVPPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI3094-KVPVERSART   PIC  X(0001).
             10 PI-DRKI3094-KVPVORNAME   PIC  X(0030).
             10 PI-DRKI3094-KVPVORVENDDATUM   PIC  X(010).
             10 PI-DRKI3094-KVPVORVGES-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI3094-KVPVORVGES-KTY   PIC S9(4) COMP.
             10 PI-DRKI3094-KVPVORVKUENDKZ   PIC  X(0001).
             10 PI-DRKI3094-KVPZUSRAHMEN   PIC  X(0001).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3094-NI1.
           05 PI-DRKI3094-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 12.
       01  PI-DRKI3094-NI7.
           05 PI-DRKI3094-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 12.
      *
       01  PO-DRKI3094.
           05  PO-DRKI3094-RC                     PIC  X(003).
               88 PO-DRKI3094-RC-OK               VALUE 'OK '.
               88 PO-DRKI3094-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

