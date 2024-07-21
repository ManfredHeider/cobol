      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0346.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0346.CPv  $
      *
      *   Rev 2.1   02 May 2001 09:27:42   t13ex49
      *AKI00161 - erstmalige Prod-Übergabe
      *
      *   Rev 2.0   Mar 16 1999 09:57:32   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0346                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0346                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  21.03.01/15.56.27
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
      *    DIESES COPY ENTHAELT DIE EIN- UND AUSGABEPARAMETER
      *    FUER DIE FUNKTION DRAT0346
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0346.
           05 PI-DRAT0346-KEY.
              10 PI-DRAT0346-FTXT-SYSCODE        PIC  X(002).
              10 PI-DRAT0346-FTXT-FEHLERNUMMER   PIC S9(004) COMP.
      *
       01  PO-DRAT0346.
           05 PO-DRAT0346-RC                     PIC  X(003).
              88 PO-DRAT0346-RC-OK                    VALUE 'OK '.
              88 PO-DRAT0346-RC-NOT-FOUND             VALUE 'NFD'.
           05 PO-DRAT0346-DATEN.
              10 PO-DRAT0346-FTXT-TEXT           PIC  X(060).
              10 PO-DRAT0346-FTXT-USERID         PIC  X(007).
      *
      ******* ENDE COPY-MEMBER **************************************
