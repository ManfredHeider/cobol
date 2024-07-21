      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  10.02.2021
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0031.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRCV0031.CPv  $
      *
      *   Rev 2.1   Feb 11 2021 12:13:24   t13ae35
      *ACV00234: VBAK-Rückstufungs-Aktion von Karriere&Vergütung - 
      *BeschaffungsPGM
      *
      *   Rev 2.0   Feb 10 2021 11:49:34   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME     *
      * --------+--------+----+----------------------------+----------*
      * VLA-1351!11.02.21!2.1 ! NEU                        ! J.Hering *
280122* VLA-2408!28.02.22! 2.2! Stufe hinzu                ! J.Hering *
      *         !        !    !                            !          *
      *         !        !    !                            !          *
      *                                                               *
      *****************************************************************
      * Parameter für UPVB0204 (VBA-Kündigungsaktion)                 *
      *---------------------------------------------------------------*
       01  DRCV0031-SATZ.
      *--- VB-Parameter
           05 DRCV0031-VBNR                     PIC 9(007).
           05 DRCV0031-PZ                       PIC X(001).
           05 DRCV0031-GEB-DAT                  PIC X(010).
           05 DRCV0031-VERTR-VOM                PIC X(010).
           05 DRCV0031-KUE-ZUM                  PIC X(010).
      *--- Betreuer-Parameter
           05 DRCV0031-BVBNR                    PIC 9(007).
           05 DRCV0031-BPZ                      PIC X(001).
           05 DRCV0031-BANRKZ                   PIC 9(001).
           05 DRCV0031-BDIRNR                   PIC 9(003).
           05 DRCV0031-BNAME                    PIC X(040).
           05 DRCV0031-BVNAME                   PIC X(020).
280122*--- VB-Stufe
           05 DRCV0031-STUFE                    PIC X(004).
      *****
      *****   ENDE DRCV0031
      *****


