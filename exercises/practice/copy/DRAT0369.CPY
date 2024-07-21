      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0369.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0369.CPv  $
      *
      *   Rev 2.1   02 May 2001 09:02:34   t13ex49
      *AKI00161 - erstmalige Prod.-Übergabe
      *
      *   Rev 2.0   22 Mar 2001 15:50:42   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0369                                      *
      *    --------------------------------------------------------   *
      *                                                               *
      *    Satzbeschreibung zum Laden von TBAT0033                    *
      *                                                               *
      *                                                               *
      *                                                               *
      *    --------------------------------------------------------   *
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00161!23.04.01!2.0 ! NEU                       !Peter Pohl*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *
      ****************************************************************
      *
       01  FEHLERREAKTION-SATZ.
              10 DRAT0369-SYSCODE            PIC  X(002).
              10 DRAT0369-JOBNAME            PIC  X(008).
              10 DRAT0369-PROGRAMMNAME       PIC  X(008).
              10 DRAT0369-FEHLERNUMMER       PIC S9(004) COMP.
              10 DRAT0369-FELDNAME           PIC  X(025).
              10 DRAT0369-RELEVANZ           PIC  X(001).
              10 DRAT0369-REAKTION           PIC  X(001).
              10 DRAT0369-REAKTIONHINWEIS    PIC  X(025).
              10 DRAT0369-LETZTEAENDERUNG    PIC  X(026).
              10 DRAT0369-USERID             PIC  X(007).
      *

