      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0357.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0357.CPv  $
      *
      *   Rev 2.1   02 May 2001 09:29:04   t13ex49
      *AKI00161 - erstmalige Prod-Übergabe
      *
      *   Rev 2.0   01 Mar 2001 13:03:16   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0357                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0357                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  21.03.01/15.56.27
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
      *    DIESES COPY ENTHAELT DIE EIN- UND AUSGABEPARAMETER
      *    FUER DIE FUNKTION DRAT0357
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0357.
           05 PI-DRAT0357-KEY.
              10 PI-DRAT0357-FTXT-SYSCODE        PIC  X(002).
              10 PI-DRAT0357-FTXT-FEHLERNUMMER   PIC S9(004) COMP.
      *
       01  PO-DRAT0357.
           05 PO-DRAT0357-RC                     PIC  X(003).
              88 PO-DRAT0357-RC-OK                    VALUE 'OK '.
              88 PO-DRAT0357-RC-NOT-FOUND             VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
