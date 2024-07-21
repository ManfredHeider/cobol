      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0544.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0544.CPv  $
      *
      *   Rev 2.0   May 25 1999 13:46:56   T13AE18
      *APA00012
      *
      *   Rev 1.1   04 Aug 1998 14:11:46   T13EX28
      *APA00032 - Uebergabe Konditionsupro fuer PEW Release 2.0
      *
      *   Rev 1.0   12 Sep 1997 17:00:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0544                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0544                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    21.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!21.10.97!    ! NEU                       ! BOHLINGER*
      * afpa0020!05.11.98!2.00! po-drpa0544-knr-pgrp-art  !bohlinger *
      *         !        !    ! eingefügt                 !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0544
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT-ALL  <<<
      *
      ****************************************************************
      *
       01  PI-DRPA0544.
           05  PI-DRPA0544-UFKT                 PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST NEXT, 3=QUIT
      *
           05  PI-DRPA0544-AUSWAHL-KRITERIEN.
               10 PI-DRPA0544-PGEB-GES          PIC S9(04) COMP.
               10 PI-DRPA0544-PROD-PROD-SCHL    PIC S9(04) COMP.
               10 PI-DRPA0544-S-PROD-SCHL       PIC  X(01).
               10 PI-DRPA0544-STICHTAG          PIC  X(10).
               10 PI-DRPA0544-S-STICHTAG        PIC  X(01).
      *
      *
       01  PO-DRPA0544.
           05  PO-DRPA0544-RC                   PIC  X(003).
               88 PO-DRPA0544-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0544-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0544-RC-LEER                      VALUE 'LER'.
      *
           05  PO-DRPA0544-ERGEBNISZEILE.
               10 PO-DRPA0544-PGRP-ID           PIC S9(004) COMP.
               10 PO-DRPA0544-KNR-PGRP-ART      PIC S9(004) COMP.
               10 PO-DRPA0544-PPGR-GUELT-AB     PIC  X(010).
               10 PO-DRPA0544-PPGR-GUELT-BIS    PIC  X(010).
               10 PO-DRPA0544-PGRP-ART-BL       PIC  X(080).
               10 PO-DRPA0544-PGRP-ART-AUSPR-BM PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************
