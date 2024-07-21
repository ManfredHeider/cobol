      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0754.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0754.CPv  $
      *
      *   Rev 2.2   Jun 09 2020 11:27:58   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.1   Aug 23 1999 11:33:06   T13EX21
      *APA00049 - Unterfunktion eingefügt
      *
      *   Rev 2.0   May 26 1999 15:25:44   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:45:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0754  Funktion Insert Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Opritz                                          *
      *    DATUM:    23.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!23.12.97! 01 ! NEUERSTELLUNG             ! OPRITZ   *
      * APA00049!22.06.99!    ! UFKT. (FÜR INSERT-QSAM)   ! STORCH   *
      *         !        !    ! EINGEFÜGT                 !          *
      * APA02908!17.02.20!2.2 ! GES-UEBERGR EVS           ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0754                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PVOR TBPA0100              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0754.
          05 PI-DRPA0754-UFKT                  PIC  9(002).
          05 PI-DRPA0754-PVOR-KEY.
170220       10 PI-DRPA0754-PGEB-GES-VERTRAG PIC S9(004) BINARY.
             10 PI-DRPA0754-PRINTE-VERTRAG   PIC S9(015) PACKED-DECIMAL.
170220       10 PI-DRPA0754-PGEB-GES-VORVER  PIC S9(004) BINARY.
             10 PI-DRPA0754-PRINTE-VORVERTRAG
                                             PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRPA0754.
          05 PO-DRPA0754-RC                   PIC  X(003).
             88 PO-DRPA0754-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0754-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************

