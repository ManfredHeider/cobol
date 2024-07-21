      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1695.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI1695.CPv  $
      *
      *   Rev 2.5   Jan 09 2020 13:51:54   t13ex39
      *AKI01747: EU-DSGVO: Vertragslöschung der Deutschen Bank
      *
      *   Rev 2.4   Jun 02 2015 17:00:24   t13ex27
      *FKI01499: Abbruch Daisy GELOESCHTEVERTRDAS
      *
      *   Rev 2.3   Oct 09 2013 17:06:52   t13ex27
      *FKI00911: Verträge löschen
      *
      *   Rev 2.2   Oct 09 2013 15:19:54   t13ex27
      *FKI00911: Verträge löschen
      *
      *   Rev 2.1   Jul 05 2004 10:35:30   t13ex39
      *AKI00082: DEUTSCHE BANK
      *
      *   Rev 2.0   Jun 23 2004 10:39:20   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1695                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1695                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         12.12.2019 / 07:31:45 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI1695
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  NEXT VALUE<<<
      *
      ****************************************************************
      *
       01  PI-DRKI1695.
           05  PI-DRKI1695-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI1695-GVDGESELLSCHAFT   PIC S9(4) COMP.
             10 PI-DRKI1695-GVDVERTRAGSNUMMER   PIC  X(0015).
END   *

           05  PI-DRKI1695-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *


           05  PI-DRKI1695-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI1695-GVDDATUM   PIC  X(010).
             10 PI-DRKI1695-GVDVERTRAGSABLAUF   PIC  X(010).
             10 PI-DRKI1695-GVDVERTRAGSID   PIC  X(0013).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI1695-NI1.
           05 PI-DRKI1695-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI1695-NI7.
           05 PI-DRKI1695-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.

      *
       01  PO-DRKI1695.
           05  PO-DRKI1695-RC                     PIC  X(003).
               88 PO-DRKI1695-RC-OK                  VALUE 'OK '.
               88 PO-DRKI1695-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
           05  PO-DRKI1695-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI1695-GVDLAUFENDENUMMER   PIC S9(4) COMP.
END   *


      *
      ******* ENDE COPY-MEMBER **************************************

