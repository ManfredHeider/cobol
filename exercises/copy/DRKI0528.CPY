      *---------------------------------------------------------------*
      *    Standard fœr alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0528.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI0528.CPv  $
      *
      *   Rev 2.5   Mar 30 2020 08:44:36   t13ae31
      *AKI01766: G81 Advocard neue Felder u.a.
      *
      *   Rev 2.4   Jul 21 2009 13:35:18   t13ex14
      *AKI00817: Ausweis von Zusatzinformationen zu Advocard; OZP-O
      *bjekte
      *
      *   Rev 1.0   07 Aug 1998 14:45:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0528                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0528                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         04.03.2020 / 13:26:47 / T13AE31
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0528
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0528.
           05  PI-DRKI0528-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI0528-VBBVERSBEDINGLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0528-VBBVERSBEDINGUNGID   PIC S9(9) COMP.
             10 PI-DRKI0528-VBBVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0528-VBBVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI0528-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI0528-NI-VBBAVB-KNR       PIC S9(4) COMP.
             15 PI-DRKI0528-NI-VBBAVB-KTY       PIC S9(4) COMP.
             15 PI-DRKI0528-N-VBBBDNGNG-KNR     PIC S9(4) COMP.
             15 PI-DRKI0528-N-VBBBDNGNG-KTY     PIC S9(4) COMP.
END   *

           05  PI-DRKI0528-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI0528-VBBAVB-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI0528-VBBAVB-KTY   PIC S9(4) COMP.
             10 PI-DRKI0528-VBBBEDINGUNG-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI0528-VBBBEDINGUNG-KTY   PIC S9(4) COMP.
             10 PI-DRKI0528-VBBVEBPARTNERGES   PIC S9(4) COMP.
             10 PI-DRKI0528-VBBVERSBEDINGKEY   PIC  X(0002).
             10 PI-DRKI0528-VBBVERSBEDINGUNG.
                49 PI-DRKI0528-VBBVRSBDNGNG-LEN PIC S9(4) COMP.
                49 PI-DRKI0528-VBBVRSBDNGNG-TEXT PIC X(50).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI0528-NI1.
           05 PI-DRKI0528-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 4.
       01  PI-DRKI0528-NI7.
           05 PI-DRKI0528-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 4.

      *
       01  PO-DRKI0528.
           05  PO-DRKI0528-RC                     PIC  X(003).
               88 PO-DRKI0528-RC-OK                  VALUE 'OK '.
               88 PO-DRKI0528-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

