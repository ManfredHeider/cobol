      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.02.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1646.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1646.CPv  $
      *
      *   Rev 2.1   Mar 11 2004 10:04:04   t13ex49
      *Einführung AdresseOrtsteil
      *
      *   Rev 2.0   Feb 20 2004 09:51:24   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1646                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1646                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  30.07.2003/10:25:37
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00268!01.03.04!    ! Update für                ! P. Pohl  *
      *         !        !    ! AdresseOrtsTeil,          !          *
      *         !        !    ! AdressePostPruef,         !          *
      *         !        !    ! AdressePostPruefID,       !          *
      *         !        !    ! AdresseSuchStrasse,       !          *
      *         !        !    ! AdresseGeprueft und       !          *
      *         !        !    ! AdresseBDSLDID sowie      !          *
      *         !        !    ! -PLZ, -ORT und -STRHAUSNUM!          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1646
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1646.
           05  PI-DRKI1646-KEY.


             10 PI-DRKI1646-ADRESSEID            PIC X(13).


           05  PI-DRKI1646-DATEN.


             10 PI-DRKI1646-ADRESSEGEPRUEFT      PIC X(01).
             10 PI-DRKI1646-ADRESSESUCHSTRASSE   PIC X(45).
             10 PI-DRKI1646-ADRESSEBDSLDID       PIC X(03).
             10 PI-DRKI1646-ADRESSEPLZ           PIC X(15).
             10 PI-DRKI1646-ADRESSEORT           PIC X(30).
             10 PI-DRKI1646-ADRESSESTRHAUSNUM    PIC X(45).
             10 PI-DRKI1646-ADRESSEORTSTEIL      PIC X(30).
             10 PI-DRKI1646-ADRESSEPOSTPRUEF     PIC X(25).
             10 PI-DRKI1646-ADRESSEPOSTPRUEFID   PIC S9(004) COMP.


      *
       01  PO-DRKI1646.
           05  PO-DRKI1646-RC                 PIC  X(003).
               88 PO-DRKI1646-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1646-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

