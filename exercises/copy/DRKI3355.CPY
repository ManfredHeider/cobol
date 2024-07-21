      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  02.07.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3355.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3355.CPv  $
      *
      *   Rev 2.3   Feb 13 2020 06:39:00   t13ex39
      *AKI01765: Deutsche Bank - Ergänzung fehlender Kreditkartenar
      *ten
      *
      *   Rev 2.2   Sep 13 2018 09:58:28   t13ae52
      *AKI01601: Kreditkarten - Umstellung
      *
      *   Rev 2.1   Sep 10 2018 09:52:26   t13ae52
      *AKI01601: Kreditkarten - Umstellung
      *
      *   Rev 2.0   Jul 02 2018 09:19:32   t13ae52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3355                                      *
      *    --------------------------------------------------------   *
      *    Cursor Full SELECT ZXKI0006 G48_KREDITKARTE                *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01601!02.07.18!2.0 ! Neuerstellung             !Schumacher*
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI01765!02.02.20!    ! G48KKALIMIT 8,2           ! WHO      *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * DBO-8798!12.07.21!    !G48MANDANT eingefuegt      !ATAR      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3355
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3355.
           05 PI-DRKI3355-UFKT-CODE           PIC  9(002).
              88 PI-DRKI3355-UFKT-FIRST            VALUE 1.
              88 PI-DRKI3355-UFKT-NEXT             VALUE 2.
              88 PI-DRKI3355-UFKT-QUIT             VALUE 3.

           05 PI-DRKI3355-KEY.                                   
              10 PI-DRKI3355-G48MANDANT       PIC  S9(009) COMP.
 
           05 PI-DRKI3355-KEY.
              10 PI-DRKI3355-G48KKAVNR        PIC  X(015).
              10 PI-DRKI3355-G48KKAART        PIC  X(003).
              10 PI-DRKI3355-G48KKANR         PIC  X(016).
 
       01  PO-DRKI3355.
           05 PO-DRKI3355-KEY.
              10 PO-DRKI3355-IG48KKAVNR       PIC  X(015).
              10 PO-DRKI3355-IG48KKAART       PIC  X(003).
              10 PO-DRKI3355-IG48KKANR        PIC  X(016).
 
           05 PO-DRKI3355-RC                  PIC  X(003).
              88 PO-DRKI3355-RC-OK            VALUE 'OK '.
              88 PO-DRKI3355-RC-LEER          VALUE 'LER'.
              88 PO-DRKI3355-RC-EOF           VALUE 'EOF'.
      *
           05 PO-DRKI3355-DATEN.
              10 PO-DRKI3355-G48KKAVNR        PIC  X(015).
              10 PO-DRKI3355-G48KKAART        PIC  X(003).
              10 PO-DRKI3355-G48KKANR         PIC  X(016).
              10 PO-DRKI3355-G48KKAHAUPTNR    PIC  X(016).
              10 PO-DRKI3355-G48KKAVNRHAUPT   PIC  X(010).
              10 PO-DRKI3355-G48KKAABRECHKTO  PIC  X(010).
              10 PO-DRKI3355-G48KKAABRECHBLZ  PIC  X(008).
              10 PO-DRKI3355-G48KKAOEFFDAT    PIC  X(008).
              10 PO-DRKI3355-G48KKASCHLDAT    PIC  X(008).
              10 PO-DRKI3355-G48KKAMAHNSTATUS PIC  X(001).
              10 PO-DRKI3355-G48KKAFILNRKTO   PIC  X(003).
              10 PO-DRKI3355-G48KKAIBAN       PIC  X(034).
              10 PO-DRKI3355-G48KKABIC        PIC  X(011).
              10 PO-DRKI3355-G48KKAMANDREF    PIC  X(035).
              10 PO-DRKI3355-G48KKANACHNAMEBZ PIC  X(028).
              10 PO-DRKI3355-G48KKADELKZ      PIC  X(001).
              10 PO-DRKI3355-G48KKAPIN        PIC  X(009).
              10 PO-DRKI3355-G48KKALIMIT      PIC S9(006)V9(002) COMP-3.
 
      ******* ENDE COPY-MEMBER ***************************************
 
 


