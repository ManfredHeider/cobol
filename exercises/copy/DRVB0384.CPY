      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0384.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRVB0384.CPv  $
      *
      *   Rev 2.7   May 20 2008 12:11:56   t13ae48
      *AVB00875: Verlängerung der Adressfelder und Kontaktdaten - P
      *rogramme (JMS) - 3
      *
      *   Rev 2.6   28 Dec 2000 12:55:34   T13AE30
      *AVB00517 - Anpassung wegen Übernahme Abrechnungsdruck durch DVA-A
      *
      *   Rev 2.5   Jan 11 2000 16:24:42   T13AE30
      *AAS00051 - Stammdaten-DTA -> Aachen
      *
      *   Rev 2.4   Sep 02 1999 12:09:52   T13AE30
      * 
      *
      *   Rev 2.3   Jun 25 1999 08:20:46   T13AE30
      *AAS00051 - Stammdaten -DTA ---> Aachen
      *
      *   Rev 2.2   Jun 25 1999 08:11:36   T13AE30
      * 
      *
      *   Rev 2.1   Jun 02 1999 14:25:40   T13AE30
      * 
      *
      *   Rev 2.0   Mar 17 1999 10:59:58   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0384                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPVB0110                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    21.04.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00051!21.04.99! 01 ! NEUERSTELLUNG             ! WILLIG   *
      * AVB00517!20.12.00! 02 ! ERWEITERUNG DER SCHNITT-  ! WILLIG   *
      *         !        !    ! STELLE                    !          *
      * AVB00875!11.03.08! 02 ! TELEFON-/TELEFAX 14->26   ! J.MAESS  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0384                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0384.
          05 PI-DRVB0384-MA-ST-NR               PIC S9(009) BINARY.
          05 PI-DRVB0384-KNR-ST-TYP             PIC S9(004) BINARY.
      *
       01 PO-DRVB0384.

          05 PO-DRVB0384-RC                     PIC  X(003).
             88 PO-DRVB0384-RC-OK                    VALUE 'OK '.
             88 PO-DRVB0384-RC-LEER                  VALUE 'LER'.
             88 PO-DRVB0384-RC-NOT-FOUND             VALUE 'NFD'.

          05 PO-DRVB0384-DATEN.
             10 PO-DRVB0384-Z-KNR-AUSL           PIC  X(004).
             10 PO-DRVB0384-Z-MAAN-PLZ           PIC  X(005).
             10 PO-DRVB0384-Z-MAAN-ORT           PIC  X(040).
             10 PO-DRVB0384-Z-MAAN-STRASSE       PIC  X(040).
110308*      10 PO-DRVB0384-Z-MAAN-TELEFON       PIC  X(014).
110308*      10 PO-DRVB0384-Z-MAAN-TELEFAX       PIC  X(014).
110308       10 PO-DRVB0384-Z-MAAN-TELEFON       PIC  X(026).
110308       10 PO-DRVB0384-Z-MAAN-TELEFAX       PIC  X(026).
             10 PO-DRVB0384-Z-MAAN-PLZ-PF        PIC  X(009).
             10 PO-DRVB0384-Z-MAAN-ORT-PF        PIC  X(040).
             10 PO-DRVB0384-Z-MAAN-POSTFACH      PIC  X(006).
             10 PO-DRVB0384-Z-KNR-GRND           PIC S9(004)  COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0384 *****************************
      *---------------------------------------------------------------*
