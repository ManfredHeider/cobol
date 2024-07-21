      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.11.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1620.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI1620.CPv  $
      *
      *   Rev 2.6   Jun 02 2009 13:39:04   t13ex44
      *APA00852: Ges. 92 - Vertragsnr.
      *
      *   Rev 2.5   Apr 11 2008 10:25:34   t13ex48
      *FKI01150: Korrektur des KD-Übertragungs-Upros für Provisions
      *-Dialog
      *
      *   Rev 2.4   11 Dec 2007 12:22:52   t13ex27
      *AKL00664: Allfinanz: Mandantenprüfung bei ID-Funktionen
      *
      *   Rev 2.3   29 Mar 2007 12:14:14   t13ex43
      *AKI00641: PGM3 - Anpassung KÜ Daisy für OKI
      *
      *   Rev 2.2   Mar 31 2004 13:55:40   t13ex39
      *FKI00847 Prüfung auf Vertrag vorhanden
      *
      *   Rev 2.1   Mar 15 2004 15:47:38   T13EX39
      *FKI00388.UE001 NEU DAISY
      *
      *   Rev 2.0   Nov 27 2003 13:57:00   T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1620                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FüR DAS UPRO UPKI0164                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    --------------------------------------------------------  *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AKI00388!01.12.03! 1.0! NEU                       ! HORNUNG  *
      * FKI00847!31.03.04! 1.1! RC-VTR-NOT-FOUND          ! HORNUNG  *
      *         !        !    ! wenn Vertrag nicht vorhan-!          *
      *         !        !    ! den                       !          *
150207* AKI00641!15.02.07! 2.3! RC-KUE-FEHLER HINZU       ! KLEEMEIER*
      *         !28.11.07! 2.4! Neuer RC bei ungleichem   !I. Frenzel*
      *         !        !    ! Mandant                   !          *
      * APA00852!20.05.09! 2.5! ALPHANUMMERISCHE VNR      ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1620
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRKI1620.
           05  PI-DRKI1620-DATEN.
             10 PI-DRKI1620-VB-NUMMER          PIC 9(07).
200509*      10 PI-DRKI1620-VNR                PIC 9(15).
200509       10 PI-DRKI1620-VNR                PIC X(15).
             10 PI-DRKI1620-GES                PIC S9(4) BINARY.
             10 PI-DRKI1620-FAELLIG-VON        PIC X(10).
      *
       01  PO-DRKI1620.
           05  PO-DRKI1620-RC                 PIC  X(003).
               88 PO-DRKI1620-RC-OK                        VALUE 'OK '.
      * KÜ ERFOLGREICH EINGESTELLT
               88 PO-DRKI1620-RC-NOT-FOUND                 VALUE 'NFD'.
      * KEIN VN IN VERTAGROLLEBEZ
               88 PO-DRKI1620-RC-INSERT                    VALUE 'INS'.
      * PERSONNEN-ID BEREITS IN CCONTROL
               88 PO-DRKI1620-RC-CHANGED                   VALUE 'CHA'.
      * NEUE VN-ID BEREITS IN VERTAGROLLEBEZ
               88 PO-DRKI1620-RC-VER-ERNEUT-UEB            VALUE 'NEU'.
      * VERTRAG ZUR ERNEUTEN ÜBERTRAGUNG VORSEHEN
               88 PO-DRKI1620-RC-GES                       VALUE 'GES'.
      * FALSCHE GESELSCHAFTSNR
310304         88 PO-DRKI1620-RC-VTR-NOT-FOUND             VALUE 'VNF'.
310304* VERTRAG NICHT VORHANDEN
150207* KUNDENÜBERTRAGUNG DURCH UPKI0203 FEHLGESCHLAGEN
150207         88 PO-DRKI1620-RC-KUE-FEHLER                VALUE 'KUF'.
281107* Ungleicher Mandant bei beteiligten VBs
281107         88 PO-DRKI1620-RC-UMA-FEHLER                VALUE 'UMA'.
150207* FEHLER-KNR AUS UPKI0203
150207     05  PO-DRKI1620-RC-KUE-FEHLER-KNR  PIC S9(004) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
      *
