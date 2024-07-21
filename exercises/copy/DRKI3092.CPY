      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.09.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3092.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3092.CPv  $
      *
      *   Rev 2.2   Dec 19 2016 13:22:32   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.1   Dec 07 2016 10:49:40   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.0   Sep 20 2016 14:14:20 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3092                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3092                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         19.12.2016 / 13:17:34 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI3092
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3092.
           05  PI-DRKI3092-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3092-KVPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3092-KVPVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI3092.
           05  PO-DRKI3092-RC                     PIC  X(003).
               88 PO-DRKI3092-RC-OK               VALUE 'OK '.
               88 PO-DRKI3092-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI3092-NI.
               10 NI-DRKI3092-TAB.
                  15 DRKI3092-ELE
                     PIC S9(04) COMP OCCURS 12 TIMES.
               10 NI-DRKI3092 REDEFINES NI-DRKI3092-TAB.
 
ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI3092-NI-KVPABRECHKZ   PIC S9(4) COMP.
             15 PO-DRKI3092-NI-KVPBEGINNDATUM   PIC S9(4) COMP.
             15 PO-DRKI3092-NI-KVPENDEDATUM   PIC S9(4) COMP.
             15 PO-DRKI3092-NI-KVPGEBDATUM   PIC S9(4) COMP.
             15 PO-DRKI3092-NI-KVPNACHNAME   PIC S9(4) COMP.
             15 PO-DRKI3092-NI-KVPVERSART   PIC S9(4) COMP.
             15 PO-DRKI3092-NI-KVPVORNAME   PIC S9(4) COMP.
             15 PO-DRKI3092-NI-KVPVORVENDDATUM   PIC S9(4) COMP.
             15 PO-DRKI3092-NI-KVPVORVGES-KNR   PIC S9(4) COMP.
             15 PO-DRKI3092-NI-KVPVORVGES-KTY   PIC S9(4) COMP.
             15 PO-DRKI3092-NI-KVPVORVKUENDKZ   PIC S9(4) COMP.
             15 PO-DRKI3092-NI-KVPZUSRAHMEN   PIC S9(4) COMP.
END   *
 
           05  PO-DRKI3092-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3092-KVPABRECHKZ   PIC  X(0001).
             10 PO-DRKI3092-KVPANREDE-KNR   PIC S9(4) COMP.
             10 PO-DRKI3092-KVPANREDE-KTY   PIC S9(4) COMP.
             10 PO-DRKI3092-KVPBEGINNDATUM   PIC  X(010).
             10 PO-DRKI3092-KVPENDEDATUM   PIC  X(010).
             10 PO-DRKI3092-KVPGEBDATUM   PIC  X(010).
             10 PO-DRKI3092-KVPNACHNAME   PIC  X(0030).
             10 PO-DRKI3092-KVPPRODUKTID   PIC S9(4) COMP.
             10 PO-DRKI3092-KVPVERSART   PIC  X(0001).
             10 PO-DRKI3092-KVPVORNAME   PIC  X(0030).
             10 PO-DRKI3092-KVPVORVENDDATUM   PIC  X(010).
             10 PO-DRKI3092-KVPVORVGES-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI3092-KVPVORVGES-KTY   PIC S9(4) COMP.
             10 PO-DRKI3092-KVPVORVKUENDKZ   PIC  X(0001).
             10 PO-DRKI3092-KVPZUSRAHMEN   PIC  X(0001).
             10 PO-DRKI3092-LETZTEAENDERUNG   PIC  X(0026).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

