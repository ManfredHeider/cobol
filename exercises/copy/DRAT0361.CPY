      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0361.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0361.CPv  $
      *
      *   Rev 2.1   02 May 2001 09:29:24   t13ex49
      *AKI00161 - erstmalige Prod-Übergabe
      *
      *   Rev 2.0   19 Mar 2001 10:37:58   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0361                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0361                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  28.03.01/14.26.54
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00161!22.03.01!2.0 ! NEU                       ! A.Blaess *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0361
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0361.
           05 PI-DRAT0361-KEY.
              10 PI-DRAT0361-FR-FEHLERNUMMER    PIC S9(004) COMP.
              10 PI-DRAT0361-FR-FELDNAME        PIC  X(025).
              10 PI-DRAT0361-FR-JOBNAME         PIC  X(008).
              10 PI-DRAT0361-FR-PROGRAMMNAME    PIC  X(008).
              10 PI-DRAT0361-FR-SYSCODE         PIC  X(002).
           05 PI-DRAT0361-DATEN.
              10 PI-DRAT0361-FR-REAKTION        PIC  X(001).
              10 PI-DRAT0361-FR-REAKTIONHINWEIS PIC  X(025).
              10 PI-DRAT0361-FR-RELEVANZ        PIC  X(001).
              10 PI-DRAT0361-FR-USERID          PIC  X(007).
      *
       01  PO-DRAT0361.
           05 PO-DRAT0361-RC                    PIC  X(003).
              88 PO-DRAT0361-RC-OK                   VALUE 'OK '.
              88 PO-DRAT0361-RC-DUPLICATE-VALUE      VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
