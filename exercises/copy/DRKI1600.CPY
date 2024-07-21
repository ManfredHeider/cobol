      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.10.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1600.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1600.CPv  $
      *
      *   Rev 2.1   Oct 28 2003 15:54:28   t13ex49
      *UPDATE ADRESSESUCHSTRASSE und ADRESSEGEPRUEFT
      *
      *   Rev 2.0   Oct 27 2003 09:26:50   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1600                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1600                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  30.07.2003/10:25:37
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00789!27.10.03!    ! Update für                ! P. Pohl  *
      *         !        !    ! AdresseSuchStrasse und    !          *
      *         !        !    ! AdresseGeprueft           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1600
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1600.
           05  PI-DRKI1600-KEY.


             10 PI-DRKI1600-ADRESSEID   PIC X(13).


           05  PI-DRKI1600-DATEN.


             10 PI-DRKI1600-ADRESSEGEPRUEFT      PIC X(01).
             10 PI-DRKI1600-ADRESSESUCHSTRASSE   PIC X(45).


      *
       01  PO-DRKI1600.
           05  PO-DRKI1600-RC                 PIC  X(003).
               88 PO-DRKI1600-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1600-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

