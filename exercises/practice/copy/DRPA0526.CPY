      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0526.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0526.CPv  $
      *
      *   Rev 2.1   May 27 1999 09:37:44   T13EX25
      *APA00042, Korr. V2.0
      *
      *   Rev 2.0   May 27 1999 08:44:52   T13EX25
      *APA00042, Erfassung V2.0
      *
      *   Rev 1.0   12 Sep 1997 17:00:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0526  PI-/PO -Bereich zu Steuerung Umrech-
      *                           nen Provisonen     (UWPA0036)      *
      *    --------------------------------------------------------- *
      *    ERSTELLT: JÄGER-SOKOLUK                                   *
      *    DATUM:    25.09.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !25.09.97! 01 ! NEUERSTELLUNG             ! JÄGER    *
      *         !03.07.98!    ! Teststatement eingefügt   ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0526                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0526.                                                
          05 PI-DRPA0526-DATEN.                                       
             10 PI-DRPA0526-STICHTAG         PIC  X(010).             
             10 PI-DRPA0526-KTY-WAEHR-VTG    PIC S9(004) BINARY.      
             10 PI-DRPA0526-KNR-WAEHR-VTG    PIC S9(004) BINARY.      
      *                                                               
       01 PO-DRPA0526.                                                
          05 PO-DRPA0526-RC                  PIC  X(003).             
             88 PO-DRPA0526-RC-OK                 VALUE 'OK '.        
             88 PO-DRPA0526-RC-NOT-OK             VALUE 'NOK'.        
      *                                                               
      ******** ENDE COPY-MEMBER **************************************