      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0559.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0559.CPv  $
      *
      *   Rev 2.1   Jul 25 2014 10:39:26   t13ex78
      *AKI01259: Kontrolle tgl. Aktualisierungsläufe über ABLAUFKON
      *TROLLE - EDI anders behandeln
      *
      *
      *   Rev 2.0   Feb 12 2013 13:26:10   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0559                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0559                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01259!25.07.14!    ! Neu                       ! TFL      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0599
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0559.
           05  PI-DRAT0559-KEY.


             10 PI-DRAT0559-ABKJOBNAME        PIC X(8).

       01  PO-DRAT0559.
           05  PO-DRAT0559-RC                 PIC  X(003).
               88 PO-DRAT0559-RC-OK           VALUE 'OK '.
               88 PO-DRAT0559-RC-NOT-FOUND    VALUE 'NFD'.


