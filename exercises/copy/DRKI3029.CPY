      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3029.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3029.CPv  $
      *
      *   Rev 2.2   Jul 28 2015 15:57:24   t13ex48
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen. Ermittlung
      *
      *   Rev 2.1   Jul 20 2015 11:32:42   t13ex48
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen: Ermittlung, Aufbereitung
      *
      *   Rev 2.0   Jul 13 2015 17:52:34   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *****************************************************************
      *    COPY-MEMBER: DRKI3029                                      *
      *    Satzbeschreibung KSDS-Datei AML geleistete Renten(LGR)     *
      *    Satzlaenge: 80 Bytes                                       *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    LOGBUCH                                                    *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME     *
      * --------!--------!----!----------------------------!----------*
      * AKI01372!01.07.15! 01 ! Neuerstellung              ! HSE      *
      * --------!--------!----!----------------------------!----------*
      *         !        !    !                            !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
 
      *
       01  LGR-KOMPLETT-SATZ.
           05  LGR-KOMPL-SA                    PIC X(01).
      * SATZART        = 'C'
           05  LGR-KOMPL-KEY.
             10 LGR-KOMPL-VNR                  PIC 9(10).
      * VERTRAGSNUMMER
             10 LGR-KOMPL-RNTBEG               PIC X(10).
      * BEGINN DER RENTE
             10  LGR-KOMPL-RNTART              PIC 9(02).
      * ART DER RENTE (AUS RENTENBESTANDSFUEHRUNG AML)
      * 1 = Zeitrente, 2 ist zu ueberlesen, 3 = Teilrente
           05  LGR-KOMPL-RNTABLAUF             PIC X(10).
      * ABLAUFDATUM DER RENTE; wenn kein Ablauf bekannt, dann '31.12.99'
           05  LGR-KOMPL-RNTZAHLW              PIC 9(02).
      * RENTENZAHLWEISE (BEIM VEP IMMER 12 = MONATLICH)
      * Muss auf DVAG-Zahlweise umgestellt werden
           05  LGR-KOMPL-RNTGSMT               PIC 9(11)V99.
      *      GESAMTE HOEHE DER RENTE
           05  FILLER                          PIC X(32).
      *--------------- ENDE COPYBOOK DRKI3029 -------------------------
 

