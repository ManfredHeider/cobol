      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.02.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0701.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0701.CPv  $
      *
      *   Rev 2.1   Mar 16 2004 11:02:58   t13ae23
      *AAS00462 - Erweitern Bewegungsstatistik VB für Hr. Franken -
      *
      *   Rev 2.0   Feb 20 2004 13:41:10   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0701  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: RAHNER                                          *
      *    DATUM:    20.02.2003                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00462!16.03.04! 2.1! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0701                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN  <<<        LESEN ERGEBNISDATEI VB IN AUSBILD. *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0701.
          10 PI-DRVB0701-UFKT                  PIC  X.
              88 PI-DRVB0701-UFKT-READ-FIRST   VALUE '1'.
              88 PI-DRVB0701-UFKT-READ-NEXT    VALUE '2'.
              88 PI-DRVB0701-UFKT-READ-LAST    VALUE '3'.
      *
       01 PO-DRVB0701.
          10 PO-DRVB0701-RC                    PIC  X(003).
             88 PO-DRVB0701-RC-OK              VALUE 'OK '.
             88 PO-DRVB0701-RC-NOT-FOUND       VALUE 'NFD'.
          10 PO-DRVB0701-WERT-6666666          PIC 9(005).
          10 PO-DRVB0701-WERT-7777777          PIC 9(005).
          10 PO-DRVB0701-WERT-8888888          PIC 9(005).
          10 PO-DRVB0701-WERT-9999999          PIC 9(005).
