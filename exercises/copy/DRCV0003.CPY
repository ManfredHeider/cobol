      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0003.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRCV0003.CPv  $
      *
      *   Rev 2.2   Jun 14 2005 11:44:28   t13ex13
      *AAS00555: Zus.führg d. Teams in VB-Vertrag 
      *
      *   Rev 2.1   Mar 01 2004 12:05:16   t13ae35
      *ACV00046 - neu
      *
      *   Rev 2.0   02 May 2001 12:03:28   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ACV00046!19.02.04!2.1 ! Neu                       ! Hering   *
      * AAS00555!31.05.05!2.2 ! Teamtrennung aufheben     !Gutfleisch
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *            Inhalt    :  Zusatzbonus-CTV-Datei                *
      *            Datei     :  CV000900 und CV001000                *
      *            Member    :  DRCV0003                             *
      *            Organisat.:  VSAM                                 *
      *            Satzlänge :  108 Byte                             *
      *            Key       :  VBNR (1-7)                           *
      *                                                              *
      *--------------------------------------------------------------*
       01  DRCV0003-SATZ.
           05 DRCV0003-VBNR                     PIC 9(007).
           05 DRCV0003-DIR-AKT                  PIC 9(004).
310505*    05 DRCV0003-TEAM                     PIC X(015).
           05 DRCV0003-ALP-SEIT                 PIC X(010).
           05 DRCV0003-ALP-EH                   PIC X(012).
           05 DRCV0003-P34C-SEIT                PIC X(010).
           05 DRCV0003-AFO                      PIC X(001).
           05 DRCV0003-DARLEHN                  PIC X(001).
           05 DRCV0003-BETR-VBNR                PIC 9(007).
           05 DRCV0003-BETR-NAME                PIC X(040).
           05 DRCV0003-BETR-ANR-KZ              PIC X(001).
           05 DRCV0003-KUEND-BEST               PIC X(001).


