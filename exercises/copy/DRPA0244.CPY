      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0244.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0244.CPv  $
      *
      *   Rev 2.0   May 26 1999 12:59:58   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:14:12   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0244  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    27.03.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0014!27.03.97! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      *         !08.09.97!    ! ANPASSUNGEN AUFGRUND VON  ! STORCH   *
      *         !        !    ! TABELLENäNDERUNGEN        !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0244                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE <<<        PGVG TBPA0020                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0244.
          05 PI-DRPA0244-LAST-UPDATE          PIC  X(026).
          05 PI-DRPA0244-PGVG-KEY.
             10 PI-DRPA0244-PGEB-GES          PIC S9(004)  BINARY.
080997*      10 PI-DRPA0244-PGVG-GUELT-AB     PIC X(010).
080997       10 PI-DRPA0244-PGVG-GUELT-AB     PIC X(026).
             10 PI-DRPA0244-KNR-VERTRIEB-GES  PIC S9(004)  BINARY.
          05 PI-DRPA0244-PGVG-DATEN.
             10 PI-DRPA0244-PGVG-GUELT-BIS    PIC X(010).
             10 PI-DRPA0244-KTY-GES           PIC S9(004)  BINARY.
             10 PI-DRPA0244-KTY-VERTRIEB-GES  PIC S9(004)  BINARY.
             10 PI-DRPA0244-PGVG-KONTO        PIC  X(010).
             10 PI-DRPA0244-PGVG-EDV-GKTO-EIN PIC S9(003)V USAGE COMP-3.
             10 PI-DRPA0244-PGVG-EDV-GKTO-AUS PIC S9(003)V USAGE COMP-3.
      *
       01 PO-DRPA0244.
          05 PO-DRPA0244-RC                  PIC  X(003).
             88 PO-DRPA0244-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0244-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0244-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
