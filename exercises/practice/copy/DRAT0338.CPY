000100*---------------------------------------------------------------*
000200*    Standard für alle Objekte unter PVCS
000300*            Datum   :  November 1995
000400*            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
000500*---------------------------------------------------------------*
000600*
000700*---------------------------------------------------------------*
000800*    Copyright 1995 ATLAS Dienstleistungs GmbH
000900*---------------------------------------------------------------*
001000*    $Workfile:   DRAT0338.CPY  $
001100*    $Revision:   2.5  $
001200*
001300*    $Log:   L:/mf/CPY/DRAT0338.CPv  $
      *
      *   Rev 2.5   Feb 06 2018 08:14:20   T13AE34
      *AAT00063: Schulung Anpassen Beispielprogramm PMAT0018.CBL 
      *
      *   Rev 2.4   Feb 05 2018 16:27:26   T13AE34
      *AAT00063: Schulung Anpassen Beispielprogramm PMAT0018.CBL
      *
      *   Rev 2.3   Jan 24 2017 13:34:26   t13ae34
      *AAT00054: Beipiel Batch-Rahmen anpassen
      *
      *   Rev 2.2   Feb 27 2008 14:17:30   t13ae34
      *FAT00031: Anpassung Beispielprogramm PMAT0018 Schulung
      *
      *   Rev 2.1   05 Sep 2000 11:46:20   T13AE34
      *Standard-Batch-Rahmen-Beispiel
      *
      *   Rev 2.0   Mar 16 1999 09:57:30   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0338                                      *
      *    --------------------------------------------------------   *
      *    SATZBESCHREIBUNG FÜR DATEI "RAHMENSCHULUNG EINGABEDATEI"   *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AATnnnnn!05.09.00!2.1 ! NEU                       ! HEIDER   *
      * AAT00054!26.01.17!2.2 ! PRÄFIX DRAT0338, 05-ER    ! HEIDER   *
      * AAT00063!05.02.18!2.4 ! +PGEB-GES PROD-PROD-SCHL  ! HEIDER   *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
           05  DRAT0338-SATZ.
               10 DRAT0338-VB-NR           PIC S9(07).
050218         10 FILLER                   PIC  X(01).
050218         10 DRAT0338-PGEB-GES        PIC S9(04).
050218         10 FILLER                   PIC  X(01).
050218         10 DRAT0338-PROD-SCHL       PIC S9(04).
050218         10 FILLER                   PIC  X(01).
               10 DRAT0338-EINHEITEN       PIC S9(04)V9(01).
      *                                           23 BYTES
      ******* ENDE COPY-MEMBER **************************************


