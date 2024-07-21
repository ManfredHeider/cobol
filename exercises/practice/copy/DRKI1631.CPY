      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.01.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1631.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1631.CPv  $
      *
      *   Rev 2.3   Sep 18 2012 16:15:32   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-Adressen
      *
      *   Rev 2.2   Sep 23 2005 09:54:54   t13ex49
      *FKI00957: Korrektur Adressprüfung
      *
      *   Rev 2.1   Feb 02 2004 07:55:34   t13ex49
      *AKI00268 - Übergabe post. Prüfung - Komplettlauf
      *
      *   Rev 2.0   Jan 26 2004 15:46:22   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI1631                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXKI0002                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Peter Pohl                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00268!26.01.04!    ! NEUERSTELLUNG             !Peter Pohl*
      * -------------------------------------------------------------*
230905* AKI00268!23.09.05!    ! Ortsteil aufgenommen      !Peter Pohl*
      * -------------------------------------------------------------*
      * AKI00886!04.11.11!    ! Umstellung für ZWKI0002   !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1631                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI1631.
          05 PI-DRKI1631-KEY.
            07 PI-DRKI1631-ADRESSELANDID          PIC X(003).
            07 PI-DRKI1631-ADRESSEPLZ             PIC X(015).
            07 PI-DRKI1631-ADRESSEORT             PIC X(040).
            07 PI-DRKI1631-ADRESSESUCHSTRASSE     PIC X(045).
            07 PI-DRKI1631-ADRESSEGEPRUEFT        PIC X(001).
230905      07 PI-DRKI1631-ADRESSEORTSTEIL        PIC X(030).
      *
       01  PO-DRKI1631.
           05  PO-DRKI1631-RC                     PIC  X(003).
               88 PO-DRKI1631-RC-OK               VALUE 'OK '.
               88 PO-DRKI1631-RC-NOT-FOUND        VALUE 'NFD'.

          05 PO-DRKI1631-DATEN.
            07 PO-DRKI1631-ADRESSEID             PIC  X(013).
      *
      ***** ENDE COPY-MEMBER DRKI1631 *****************************
