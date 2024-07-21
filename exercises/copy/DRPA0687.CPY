      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0687.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0687.cpv  $
      *
      *   Rev 2.0   May 27 1999 11:22:28   T13EX25
      *APA00042, Erstellung V2.0
      *
      *   Rev 1.0   11 Nov 1997 17:00:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0687 FUNKTION PR O.K. setzen in PNAP     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J. Jäger-Sokoluk                                *
      *    DATUM:    15.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!15.12.97!01.01!NEU                       ! Jäger    *
      *         !        !     !                          !          *
      * APA00012!10.11.98!01.02!Schalter hinzugefügt      ! Jäger    *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0687                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0687.                                                
          05 PI-DRPA0687-DATEN.                                       
             10 PI-DRPA0687-PNAP-ID       PIC S9(015) PACKED-DECIMAL. 
             10 PI-DRPA0687-PNAP-VERSION  PIC S9(004) BINARY.         
      *                                                               
       01 PO-DRPA0687.                                                
          05 PO-DRPA0687-RC               PIC  X(003).                
             88 PO-DRPA0687-RC-OK              VALUE 'OK '.           
          05 PO-DRPA0687-NULL-SCHALTER    PIC  X.                     
      *                                                               
      ******** Ende Copy-Member **************************************