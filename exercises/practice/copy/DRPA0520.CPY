      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0520.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0520.CPv  $
      *
      *   Rev 2.1   May 27 1999 09:17:54   T13EX25
      *APA00042, Korrektur V2.0
      *
      *   Rev 2.0   May 27 1999 08:41:14   T13EX25
      *APA00042, Erfassung V2.0
      *
      *   Rev 1.0   12 Sep 1997 16:59:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0520  FUNKTION Währung des PE-Empfängers *
      *                                    ermitteln (UPPA0066)      *
      *    --------------------------------------------------------- *
      *    ERSTELLT: JÄGER-SOKOLUK                                   *
      *    DATUM:    23.09.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !23.09.97! 01 ! NEUERSTELLUNG             ! JÄGER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0520                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0520.                                                
          05 PI-DRPA0520-DATEN.                                       
             10 PI-DRPA0520-ST-NR            PIC S9(009)  BINARY.     
      *                                                               
       01 PO-DRPA0520.                                                
          05 PO-DRPA0520-RC                  PIC  X(003).             
             88 PO-DRPA0520-RC-OK                 VALUE 'OK '.        
             88 PO-DRPA0520-RC-NOT-FOUND          VALUE 'NFD'.        
          05 PO-DRPA0520-DATEN.                                       
             10 PO-DRPA0520-KTY-WAEHRUNG     PIC S9(004)  BINARY.     
             10 PO-DRPA0520-KNR-WAEHRUNG     PIC S9(004)  BINARY.     
      *                                                               
      ******** ENDE COPY-MEMBER **************************************