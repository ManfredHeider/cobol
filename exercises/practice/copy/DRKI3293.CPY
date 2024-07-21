      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3293.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3293.CPv  $
      *
      *   Rev 2.2   Dec 15 2020 15:13:44   t13ae31
      *AKI01812: G21 Leben Basis Viridium DB2
      *
      *   Rev 2.1   Apr 13 2018 10:14:34   t13ex19
      *AKI01570: FW - Generali-Partnergesellschaften - Basisdaten-V
      *ertrag
      *
      *   Rev 2.0   Feb 22 2018 15:51:51 T13EX19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3293                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3293                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.12.2020 / 11:15:37 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI3293
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3293.
           05  PI-DRKI3293-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI3293-GENVEZPNR   PIC S9(4) COMP.
             10 PI-DRKI3293-GENVGESELLSCHID   PIC  X(0002).
             10 PI-DRKI3293-GENVVNR   PIC  X(0015).
END   *

           05  PI-DRKI3293-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI3293-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3293-GENVABLAUF   PIC  X(0008).
             10 PI-DRKI3293-GENVAENDKZ   PIC  X(0001).
             10 PI-DRKI3293-GENVAKTWERT   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI3293-GENVAMTLKENNZ   PIC  X(0012).
             10 PI-DRKI3293-GENVBEGINN   PIC  X(0008).
             10 PI-DRKI3293-GENVBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI3293-GENVFLAECHE   PIC S9(4) COMP.
             10 PI-DRKI3293-GENVGARRENTE   PIC S9(6)V COMP-3.
             10 PI-DRKI3293-GENVGUTHABEN   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI3293-GENVGZWEIG   PIC  X(0002).
             10 PI-DRKI3293-GENVMIGKZN   PIC  X(0001).
             10 PI-DRKI3293-GENVNETTOBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI3293-GENVPRNUMMER   PIC S9(9) COMP.
             10 PI-DRKI3293-GENVPRODUKT   PIC  X(0050).
             10 PI-DRKI3293-GENVRENTENBEZUG   PIC  X(0001).
             10 PI-DRKI3293-GENVSPARTE   PIC  X(0003).
             10 PI-DRKI3293-GENVSTAND   PIC  X(0008).
             10 PI-DRKI3293-GENVSUMME   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI3293-GENVTARIF   PIC  X(0050).
             10 PI-DRKI3293-GENVVBNR   PIC S9(9) COMP.
             10 PI-DRKI3293-GENVVERARB-KZN   PIC  X(0001).
             10 PI-DRKI3293-GENVVORRENTE   PIC S9(6)V COMP-3.
             10 PI-DRKI3293-GENVWAGNISKZ   PIC S9(4) COMP.
             10 PI-DRKI3293-GENVZAHLWEISE   PIC  X(0002).
             10 PI-DRKI3293-GENVZUSTAND   PIC S9(4) COMP.
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3293-NI1.
           05 PI-DRKI3293-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI3293-NI7.
           05 PI-DRKI3293-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.

      *
       01  PO-DRKI3293.
           05  PO-DRKI3293-RC                     PIC  X(003).
               88 PO-DRKI3293-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3293-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

