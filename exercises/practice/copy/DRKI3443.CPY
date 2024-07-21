      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3443.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3443.CPv  $
      *
      *   Rev 2.3   Nov 22 2018 10:15:30   t13ex78
      *AKI01615: OZPs für G42_SPARPLAN
      *
      *   Rev 2.2   Oct 27 2018 15:23:48   t13ex78
      *AKI01615: OZPs für G42_FSA
      *
      *   Rev 2.1   Oct 25 2018 14:44:00   t13ex78
      *AKI01615: OZPs für G42_SPARPLAN
      *
      *   Rev 2.0   Sep 7 2018 11:29:36 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3443                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3443                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         12.08.2021 / 08:56:25 / U130685
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DBO-8960!30.08.21!    !Neues Attribut             ! ATAR     *
      *         !        !    !PI-DRKI3443-G42SSTATUS     !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3443
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3443.
           05  PI-DRKI3443-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3443-G42SLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3443-G42SVERTRAGSNR   PIC  X(0015).
END   *

           05  PI-DRKI3443-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI3443-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3443-G42SAAUFSCHLAKT   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI3443-G42SAAUFSCHLURSPR   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI3443-G42SBEGDATDYNAM   PIC  X(0008).
             10 PI-DRKI3443-G42SBIC   PIC  X(0011).
             10 PI-DRKI3443-G42SBONIFIKATION   PIC S9(3)V9(4) COMP-3.
             10 PI-DRKI3443-G42SDATBEGINNAUS   PIC  X(0008).
             10 PI-DRKI3443-G42SDATENDEAUS   PIC  X(0008).
             10 PI-DRKI3443-G42SDATVERAUFLOE   PIC  X(0008).
             10 PI-DRKI3443-G42SDYNAMIK   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI3443-G42SEINAUSDATUM   PIC  X(0002).
             10 PI-DRKI3443-G42SEINAUSTYP   PIC  X(0002).
             10 PI-DRKI3443-G42SEINZAHFRISTEND   PIC  X(0008).
             10 PI-DRKI3443-G42SENDEDAT   PIC  X(0008).
             10 PI-DRKI3443-G42SIBAN   PIC  X(0034).
             10 PI-DRKI3443-G42SISIN   PIC  X(0012).
             10 PI-DRKI3443-G42SKONTOINHABER   PIC  X(0030).
             10 PI-DRKI3443-G42SKOSTENPROZ   PIC S9(1)V9(2) COMP-3.
             10 PI-DRKI3443-G42SKUENDMERKMAL   PIC  X(0001).
             10 PI-DRKI3443-G42SMANDREF   PIC  X(0035).
             10 PI-DRKI3443-G42SOEFDAT   PIC  X(0008).
             10 PI-DRKI3443-G42SSPARRATE   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3443-G42SSTATUS   PIC  X(0001).
             10 PI-DRKI3443-G42SURSPARRATE   PIC S9(4)V9(2) COMP-3.
             10 PI-DRKI3443-G42SURSPRSPARLST   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI3443-G42SVERTLAUFZEIT   PIC S9(4) COMP.
             10 PI-DRKI3443-G42SWKN   PIC  X(0006).
             10 PI-DRKI3443-G42SZAHLWEISE   PIC  X(0001).
             10 PI-DRKI3443-G42SZLASTRUECK   PIC S9(4) COMP.
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3443-NI1.
           05 PI-DRKI3443-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3443-NI7.
           05 PI-DRKI3443-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3443.
           05  PO-DRKI3443-RC                     PIC  X(003).
               88 PO-DRKI3443-RC-OK               VALUE 'OK '.
               88 PO-DRKI3443-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************