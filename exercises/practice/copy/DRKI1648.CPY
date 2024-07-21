      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.02.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1648.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1648.CPv  $
      *
      *   Rev 2.1   Mar 11 2004 10:04:22   t13ex49
      *Einführung AdresseOrtsteil
      *
      *   Rev 2.0   Feb 26 2004 08:45:34   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1648                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1648                    *
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
      *         !        !    ! AdresseSuchStrasse,       !          *
      *         !        !    ! AdresseGeprueft und       !          *
      *         !        !    ! AdresseBDSLDID  und       !          *
      *         !        !    ! Adresseortsteil           !          *
      *         !        !    ! Adressepostpruef          !          *
      *         !        !    ! Adressepostpruefid        !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1648
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1648.
           05  PI-DRKI1648-KEY.


             10 PI-DRKI1648-ADRESSEID   PIC X(13).


           05  PI-DRKI1648-DATEN.


             10 PI-DRKI1648-ADRESSEGEPRUEFT      PIC X(01).
             10 PI-DRKI1648-ADRESSESUCHSTRASSE   PIC X(45).
             10 PI-DRKI1648-ADRESSEBDSLDID       PIC X(03).
             10 PI-DRKI1648-ADRESSEORTSTEIL      PIC X(30).
             10 PI-DRKI1648-ADRESSEPOSTPRUEF     PIC X(25).
             10 PI-DRKI1648-ADRESSEPOSTPRUEFID   PIC S9(004) COMP.


      *
       01  PO-DRKI1648.
           05  PO-DRKI1648-RC                 PIC  X(003).
               88 PO-DRKI1648-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1648-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

