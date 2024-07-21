      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0316.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0316.CPv  $
      *
      *   Rev 2.2   10 Jul 2000 15:18:36   T13AE34
      *AVB00487-Hat VB einger. Geschäft im 
      *Jahr JHJJ?
      *
      *   Rev 2.1   16 Jun 2000 11:29:22   T13AE34
      *AVB00483-Dialog VB51 
      *
      *   Rev 2.0   Feb 27 1999 15:58:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   09 Jul 1998 09:58:02   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0316  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: RAHNER                                          *
      *    DATUM:    27.03.2000                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00483!27.03.00! 2.1! NEUERSTELLUNG             ! RAHNER   *
      * AVB00487!06.07.00! 2.2! UFKT. READ-LAST           ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0316                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN  <<<        LESEN SOLL-IST-DATEI               *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0316.
          10 PI-DRVB0316-UFKT                  PIC  X.
              88 PI-DRVB0316-UFKT-READ-FIRST   VALUE '1'.
              88 PI-DRVB0316-UFKT-READ-NEXT    VALUE '2'.
060700        88 PI-DRVB0316-UFKT-READ-LAST    VALUE '3'.
          10 PI-DRVB0316-VBNR                  PIC 9(007) COMP-3.
          10 PI-DRVB0316-ISTATMON              PIC 9(005) COMP-3.
          10 PI-DRVB0316-ISTATSCH              PIC 9(002).
      *
       01 PO-DRVB0316.
          10 PO-DRVB0316-RC                    PIC  X(003).
             88 PO-DRVB0316-RC-OK              VALUE 'OK '.
             88 PO-DRVB0316-RC-NOT-FOUND       VALUE 'NFD'.
          10 PO-DRVB0316-IGRAEINH              PIC S9(8)V9 COMP-3.
          10 PO-DRVB0316-IEAEINH               PIC S9(8)V9 COMP-3.

