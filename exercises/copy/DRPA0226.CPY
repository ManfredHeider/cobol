      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0226.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0226.CPv  $
      *
      *   Rev 2.0   May 26 1999 12:53:52   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:11:58   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0226                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0226                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    27.02.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0014!27.02.97!    ! NEU                       ! BOHLINGER*
      *         !18.08.97!    ! KORREKTUREN AUFGRUND VON  ! STORCH   *
      *         !        !    ! TABELLENäNDERUNGEN        !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0226
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    JOIN AUF TABELLE PGEB UND KNR
      *                     (TBPA0008 UND TBAT0008)
      ****************************************************************
      *
       01  PI-DRPA0226.
           05  PI-DRPA0226-PROGRAMM            PIC  X(008).
           05  PI-DRPA0226-UFKT-CODE           PIC  X(02).
      *
           05  PI-DRPA0226-AUSWAHL-KRITERIEN.
               10 PI-DRPA0226-PGEB-GES           PIC S9(004) BINARY.
               10 PI-DRPA0226-PGEB-GES-GUELT-BIS PIC  X(010).
      *
       01  PO-DRPA0226.
           05  PO-DRPA0226-RC                 PIC  X(003).
               88 PO-DRPA0226-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0226-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0226-RC-LEER                      VALUE 'LER'.
      *
           05  PO-DRPA0226-DATEN.
               10 PO-DRPA0226-PGEB-GES          PIC S9(004) BINARY.
180897*        10 PO-DRPA0226-PGEB-GES-GUELT-AB PIC  X(010).
180897         10 PO-DRPA0226-PGEB-GES-GUELT-AB PIC  X(026).
               10 PO-DRPA0226-PGEB-LAST-UPDATE  PIC  X(026).
180897         10 PO-DRPA0226-PGEB-KENNUNG      PIC  X(008).
               10 PO-DRPA0226-KNR-AUSL          PIC  S9(004) BINARY.
               10 PO-DRPA0226-KTY-AUSL          PIC  S9(004) BINARY.
               10 PO-DRPA0226-KNR-BEWERT-ART    PIC  S9(004) BINARY.
               10 PO-DRPA0226-KTY-BEWERT-ART    PIC  S9(004) BINARY.
               10 PO-DRPA0226-KNR-LAST-UPDATE   PIC   X(026).
               10 PO-DRPA0226-KNR-BEZ-GES       PIC   X(080).
               10 PO-DRPA0226-KNR-BEZ10-GES     PIC   X(010).
               10 PO-DRPA0226-KNR-BEZ-KURZ-GES  PIC   X(004).
               10 PO-DRPA0226-KNR-BEZ-KURZ-AUSL PIC   X(004).
               10 PO-DRPA0226-KNR-BEZ10-BEW     PIC   X(010).
      ******* ENDE COPY-MEMBER **************************************
