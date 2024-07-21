      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0360.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0360.CPv  $
      *
      *   Rev 2.1   02 May 2001 09:29:20   t13ex49
      *AKI00161 - erstmalige Prod-Übergabe
      *
      *   Rev 2.0   01 Mar 2001 13:03:18   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0360                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0360                    *
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
      *    FÜR DIE FUNKTION DRAT0360
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0360.
           05 PI-DRAT0360-KEY.
              10 PI-DRAT0360-FR-FEHLERNUMMER  PIC S9(004) COMP.
              10 PI-DRAT0360-FR-FELDNAME      PIC  X(025).
              10 PI-DRAT0360-FR-JOBNAME       PIC  X(008).
              10 PI-DRAT0360-FR-PROGRAMMNAME  PIC  X(008).
              10 PI-DRAT0360-FR-SYSCODE       PIC  X(002).
      *
       01  PO-DRAT0360.
           05 PO-DRAT0360-RC                  PIC  X(003).
              88 PO-DRAT0360-RC-OK                 VALUE 'OK '.
              88 PO-DRAT0360-RC-NOT-FOUND          VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
