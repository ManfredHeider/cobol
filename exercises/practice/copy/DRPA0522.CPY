      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0522.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0522.CPv  $
      *
      *   Rev 2.1   May 27 1999 09:30:08   T13EX25
      *APA00042,Korr. V2.0
      *
      *   Rev 2.0   May 27 1999 08:42:08   T13EX25
      *APA00042, Erfassung V2.0
      *
      *   Rev 1.0   12 Sep 1997 16:59:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0522  FUNKTION Umrechnungskurs           *
      *                                    ermitteln (UPPA0067)      *
      *    --------------------------------------------------------- *
      *    ERSTELLT: JÄGER-SOKOLUK                                   *
      *    DATUM:    24.09.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !24.09.97! 01 ! NEUERSTELLUNG             ! JÄGER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0522                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0522.                                                
          05 PI-DRPA0522-DATEN.                                       
             10 PI-DRPA0522-STICHTAG         PIC  X(010).             
             10 PI-DRPA0522-KTY-WAEHRUNG     PIC S9(004) BINARY.      
             10 PI-DRPA0522-KNR-WAEHR-VON    PIC S9(004) BINARY.      
             10 PI-DRPA0522-KNR-WAEHR-IN     PIC S9(004) BINARY.      
      *                                                               
       01 PO-DRPA0522.                                                
          05 PO-DRPA0522-RC                  PIC  X(003).             
             88 PO-DRPA0522-RC-OK                 VALUE 'OK '.        
          05 PO-DRPA0522-DATEN.                                       
             10 PO-DRPA0522-KURS             PIC S9(004)V9(5)         
                                                 PACKED-DECIMAL.      
      *                                                               
      ******** ENDE COPY-MEMBER **************************************