      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  31.10.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1608.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1608.CPv  $
      *
      *   Rev 2.1   Nov 13 2003 16:02:36   t13ex49
      *Vorbereitung post. Prüfung
      *
      *   Rev 2.0   Oct 31 2003 09:27:06   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1608                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1608                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  30.07.2003/10:25:37
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00268!31.10.03!    ! Update für                ! P. Pohl  *
      *         !        !    ! AdresseSuchStrasse,       !          *
      *         !        !    ! AdresseGeprueft und       !          *
      *         !        !    ! AdresseBDSLDID            !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1608
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1608.
           05  PI-DRKI1608-KEY.


             10 PI-DRKI1608-ADRESSEID   PIC X(13).


           05  PI-DRKI1608-DATEN.


             10 PI-DRKI1608-ADRESSEGEPRUEFT      PIC X(01).
             10 PI-DRKI1608-ADRESSESUCHSTRASSE   PIC X(45).
             10 PI-DRKI1608-ADRESSEBDSLDID       PIC X(03).


      *
       01  PO-DRKI1608.
           05  PO-DRKI1608-RC                 PIC  X(003).
               88 PO-DRKI1608-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1608-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

