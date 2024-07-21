      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0524.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0524.CPv  $
      *
      *   Rev 2.1   May 27 1999 09:33:20   T13EX25
      *APA00042, Korr. V2.0
      *
      *   Rev 2.0   May 27 1999 08:43:10   T13EX25
      *APA00042, Erfassung V2.0
      *
      *   Rev 1.0   12 Sep 1997 16:59:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0524  FUNKTION Beträge in Währung        *
      *                                    ermitteln (UWPA0036)      *
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
      *    FÜR DIE FUNKTION DRPA0524                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0524.                                                
          05 PI-DRPA0524-DATEN.                                       
             10 PI-DRPA0524-PROV              PIC S9(009)V99          
                                                  PACKED-DECIMAL.     
             10 PI-DRPA0524-RCK               PIC S9(009)V99          
                                                  PACKED-DECIMAL.     
             10 PI-DRPA0524-KURS              PIC S9(004)V9(5)        
                                                  PACKED-DECIMAL.     
      *                                                               
       01 PO-DRPA0524.                                                
          05 PO-DRPA0524-RC                   PIC  X(003).            
             88 PO-DRPA0524-RC-OK                  VALUE 'OK '.       
             88 PO-DRPA0524-RC-NOK                 VALUE 'NOK'.       
          05 PO-DRPA0524-DATEN.                                       
             10 PO-DRPA0524-PROV-WHG          PIC S9(009)V99          
                                                  PACKED-DECIMAL.     
             10 PO-DRPA0524-RCK-WHG           PIC S9(009)V99          
                                                  PACKED-DECIMAL.     
      *                                                               
      ******** ENDE COPY-MEMBER **************************************