      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0348.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0348.CPv  $
      *
      *   Rev 2.1   02 May 2001 09:27:52   t13ex49
      *AKI00161 - erstmalige Prod-Übergabe
      *
      *   Rev 2.0   Mar 16 1999 09:57:32   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0348                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0348                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  28.03.01/14.26.54
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00161!20.02.01!2.0 !Neu für generiertes OZP    ! A.Blaess *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0348
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0348.
           05 PI-DRAT0348-KEY.
              10 PI-DRAT0348-FR-FEHLERNUMMER    PIC S9(004) COMP.
              10 PI-DRAT0348-FR-FELDNAME        PIC  X(025).
              10 PI-DRAT0348-FR-JOBNAME         PIC  X(008).
              10 PI-DRAT0348-FR-PROGRAMMNAME    PIC  X(008).
              10 PI-DRAT0348-FR-SYSCODE         PIC  X(002).
      *
       01  PO-DRAT0348.
           05 PO-DRAT0348-RC                    PIC  X(003).
              88 PO-DRAT0348-RC-OK                   VALUE 'OK '.
              88 PO-DRAT0348-RC-NOT-FOUND            VALUE 'NFD'.
      *
           05 PO-DRAT0348-DATEN.
              10 PO-DRAT0348-FR-REAKTION        PIC  X(001).
              10 PO-DRAT0348-FR-REAKTIONHINWEIS PIC  X(025).
              10 PO-DRAT0348-FR-RELEVANZ        PIC  X(001).
              10 PO-DRAT0348-FR-USERID          PIC  X(007).
      *
      ******* ENDE COPY-MEMBER **************************************
