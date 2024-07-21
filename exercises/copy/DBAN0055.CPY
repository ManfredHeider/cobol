      *   MICRO FOCUS ENTERPRISE DEVELOPER FOR ECLIPSE  5.0.00256
      *   MICRO FOCUS BMS SCREEN PAINTER (VER BMSIDE_5-0-01)
      *   MAPSET NAME   BAN0055
      *   DATE CREATED  01/25/2023
      *   TIME CREATED  15:21:10

      *  INPUT DATA FOR MAP BAN0055
         01 BAN0055I.
            03 FILLER                         PIC X(12).
            03 TVBNRL                         PIC S9(4) COMP.
            03 TVBNRF                         PIC X.
            03 FILLER REDEFINES TVBNRF.
               05 TVBNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TVBNRI                         PIC X(2).
            03 AVBNRL                         PIC S9(4) COMP.
            03 AVBNRF                         PIC X.
            03 FILLER REDEFINES AVBNRF.
               05 AVBNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 AVBNRI                         PIC X(7).
            03 TVMNRL                         PIC S9(4) COMP.
            03 TVMNRF                         PIC X.
            03 FILLER REDEFINES TVMNRF.
               05 TVMNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TVMNRI                         PIC X(2).
            03 AVMNRL                         PIC S9(4) COMP.
            03 AVMNRF                         PIC X.
            03 FILLER REDEFINES AVMNRF.
               05 AVMNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 AVMNRI                         PIC X(7).
            03 TPINL                          PIC S9(4) COMP.
            03 TPINF                          PIC X.
            03 FILLER REDEFINES TPINF.
               05 TPINA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TPINI                          PIC X(7).
            03 AVERNRL                        PIC S9(4) COMP.
            03 AVERNRF                        PIC X.
            03 FILLER REDEFINES AVERNRF.
               05 AVERNRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AVERNRI                        PIC X(15).

            03 GRP0001I OCCURS 5.

               05 TNUML                         PIC S9(4) COMP.
               05 TNUMF                         PIC X.
               05 FILLER REDEFINES TNUMF.
                  07 TNUMA                         PIC X.
               05 FILLER                         PIC X(1).
               05 TNUMI                         PIC X(2).
               05 TKDNAML                       PIC S9(4) COMP.
               05 TKDNAMF                       PIC X.
               05 FILLER REDEFINES TKDNAMF.
                  07 TKDNAMA                       PIC X.
               05 FILLER                         PIC X(1).
               05 TKDNAMI                       PIC X(6).
               05 AKDNAML                       PIC S9(4) COMP.
               05 AKDNAMF                       PIC X.
               05 FILLER REDEFINES AKDNAMF.
                  07 AKDNAMA                       PIC X.
               05 FILLER                         PIC X(1).
               05 AKDNAMI                       PIC X(23).
               05 TKDVNAL                       PIC S9(4) COMP.
               05 TKDVNAF                       PIC X.
               05 FILLER REDEFINES TKDVNAF.
                  07 TKDVNAA                       PIC X.
               05 FILLER                         PIC X(1).
               05 TKDVNAI                       PIC X(7).
               05 AKDVNAL                       PIC S9(4) COMP.
               05 AKDVNAF                       PIC X.
               05 FILLER REDEFINES AKDVNAF.
                  07 AKDVNAA                       PIC X.
               05 FILLER                         PIC X(1).
               05 AKDVNAI                       PIC X(23).
               05 TKDSTAL                       PIC S9(4) COMP.
               05 TKDSTAF                       PIC X.
               05 FILLER REDEFINES TKDSTAF.
                  07 TKDSTAA                       PIC X.
               05 FILLER                         PIC X(1).
               05 TKDSTAI                       PIC X(7).
               05 AKDSTAL                       PIC S9(4) COMP.
               05 AKDSTAF                       PIC X.
               05 FILLER REDEFINES AKDSTAF.
                  07 AKDSTAA                       PIC X.
               05 FILLER                         PIC X(1).
               05 AKDSTAI                       PIC X(3).
               05 TPLZL                         PIC S9(4) COMP.
               05 TPLZF                         PIC X.
               05 FILLER REDEFINES TPLZF.
                  07 TPLZA                         PIC X.
               05 FILLER                         PIC X(1).
               05 TPLZI                         PIC X(10).
               05 APLZL                         PIC S9(4) COMP.
               05 APLZF                         PIC X.
               05 FILLER REDEFINES APLZF.
                  07 APLZA                         PIC X.
               05 FILLER                         PIC X(1).
               05 APLZI                         PIC X(5).
               05 AKDORTL                       PIC S9(4) COMP.
               05 AKDORTF                       PIC X.
               05 FILLER REDEFINES AKDORTF.
                  07 AKDORTA                       PIC X.
               05 FILLER                         PIC X(1).
               05 AKDORTI                       PIC X(21).
               05 AAUSLL                        PIC S9(4) COMP.
               05 AAUSLF                        PIC X.
               05 FILLER REDEFINES AAUSLF.
                  07 AAUSLA                        PIC X.
               05 FILLER                         PIC X(1).
               05 AAUSLI                        PIC X(3).
               05 TKDGEBL                       PIC S9(4) COMP.
               05 TKDGEBF                       PIC X.
               05 FILLER REDEFINES TKDGEBF.
                  07 TKDGEBA                       PIC X.
               05 FILLER                         PIC X(1).
               05 TKDGEBI                       PIC X(6).
               05 AKDGEBL                       PIC S9(4) COMP.
               05 AKDGEBF                       PIC X.
               05 FILLER REDEFINES AKDGEBF.
                  07 AKDGEBA                       PIC X.
               05 FILLER                         PIC X(1).
               05 AKDGEBI                       PIC X(6).
               05 TKDPEPL                       PIC S9(4) COMP.
               05 TKDPEPF                       PIC X.
               05 FILLER REDEFINES TKDPEPF.
                  07 TKDPEPA                       PIC X.
               05 FILLER                         PIC X(1).
               05 TKDPEPI                       PIC X(3).
               05 AKDPEPL                       PIC S9(4) COMP.
               05 AKDPEPF                       PIC X.
               05 FILLER REDEFINES AKDPEPF.
                  07 AKDPEPA                    PIC X.
               05 FILLER                       PIC X(1).
               05 AKDPEPI                      PIC X(1).
               05 TKDDILL                      PIC S9(4) COMP.
               05 TKDDILF                      PIC X.
               05 FILLER REDEFINES TKDDILF.
                  07 TKDDILA                       PIC X.
               05 FILLER                         PIC X(1).
               05 TKDDILI                       PIC X(3).
               05 AKDDILL                       PIC S9(4) COMP.
               05 AKDDILF                         PIC X.
               05 FILLER REDEFINES AKDDILF.
                  07 AKDDILA                       PIC X.
               05 FILLER                         PIC X(1).
               05 AKDDILI                       PIC X(1).
               05 TKBGRPL                       PIC S9(4) COMP.
               05 TKBGRPF                       PIC X.
               05 FILLER REDEFINES TKBGRPF.
                  07 TKBGRPA                       PIC X.
               05 FILLER                         PIC X(1).
               05 TKBGRPI                       PIC X(5).
               05 AKBGRPL                       PIC S9(4) COMP.
               05 AKBGRPF                       PIC X.
               05 FILLER REDEFINES AKBGRPF.
                  07 AKBGRPA                       PIC X.
               05 FILLER                         PIC X(1).
               05 AKBGRPI                       PIC X(30).
               05 TKBRANL                       PIC S9(4) COMP.
               05 TKBRANF                       PIC X.
               05 FILLER REDEFINES TKBRANF.
                  07 TKBRANA                       PIC X.
               05 FILLER                         PIC X(1).
               05 TKBRANI                       PIC X(7).
               05 AKBRANL                       PIC S9(4) COMP.
               05 AKBRANF                       PIC X.
               05 FILLER REDEFINES AKBRANF.
                  07 AKBRANA                       PIC X.
               05 FILLER                         PIC X(1).
               05 AKBRANI                       PIC X(31).

            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER REDEFINES AFCZANZF.
               05 AFCZANZA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  OUTPUT DATA FOR MAP BAN0055
         01 BAN0055O REDEFINES BAN0055I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 TVBNRH                         PIC X.
            03 TVBNRO                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 AVBNRH                         PIC X.
            03 AVBNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 TVMNRH                         PIC X.
            03 TVMNRO                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 AVMNRH                         PIC X.
            03 AVMNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 TPINH                          PIC X.
            03 TPINO                          PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVERNRH                        PIC X.
            03 AVERNRO                        PIC X(15).

            03 GRP0001I OCCURS 5.

               05 FILLER                        PIC X(3).
               05 TNUMH                          PIC X.
               05 TNUMO                          PIC X(2).
               05 FILLER                         PIC X(3).
               05 TKDNAMH                        PIC X.
               05 TKDNAMO                        PIC X(6).
               05 FILLER                         PIC X(3).
               05 AKDNAMH                        PIC X.
               05 AKDNAMO                        PIC X(23).
               05 FILLER                         PIC X(3).
               05 TKDVNAH                        PIC X.
               05 TKDVNAO                        PIC X(7).
               05 FILLER                         PIC X(3).
               05 AKDVNAH                        PIC X.
               05 AKDVNAO                        PIC X(23).
               05 FILLER                         PIC X(3).
               05 TKDSTAH                        PIC X.
               05 TKDSTAO                        PIC X(7).
               05 FILLER                         PIC X(3).
               05 AKDSTAH                        PIC X.
               05 AKDSTAO                        PIC X(3).
               05 FILLER                         PIC X(3).
               05 TPLZH                          PIC X.
               05 TPLZO                          PIC X(10).
               05 FILLER                         PIC X(3).
               05 APLZH                          PIC X.
               05 APLZO                          PIC X(5).
               05 FILLER                         PIC X(3).
               05 AKDORTH                        PIC X.
               05 AKDORTO                        PIC X(21).
               05 FILLER                         PIC X(3).
               05 AAUSLH                         PIC X.
               05 AAUSLO                         PIC X(3).
               05 FILLER                         PIC X(3).
               05 TKDGEBH                        PIC X.
               05 TKDGEBO                        PIC X(6).
               05 FILLER                         PIC X(3).
               05 AKDGEBH                        PIC X.
               05 AKDGEBO                        PIC X(6).
               05 FILLER                         PIC X(3).
               05 TKDPEPH                        PIC X.
               05 TKDPEPO                        PIC X(3).
               05 FILLER                         PIC X(3).
               05 AKDPEPH                        PIC X.
               05 AKDPEPO                        PIC X(1).
               05 FILLER                         PIC X(3).
               05 TKDDILH                        PIC X.
               05 TKDDILO                        PIC X(3).
               05 FILLER                         PIC X(3).
               05 AKDDILH                        PIC X.
               05 AKDDILO                        PIC X(1).
               05 FILLER                         PIC X(3).
               05 TKBGRPH                        PIC X.
               05 TKBGRPO                        PIC X(5).
               05 FILLER                         PIC X(3).
               05 AKBGRPH                        PIC X.
               05 AKBGRPO                        PIC X(30).
               05 FILLER                         PIC X(3).
               05 TKBRANH                        PIC X.
               05 TKBRANO                        PIC X(7).
               05 FILLER                         PIC X(3).
               05 AKBRANH                        PIC X.
               05 AKBRANO                        PIC X(31).

            03 FILLER                         PIC X(3).
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).



