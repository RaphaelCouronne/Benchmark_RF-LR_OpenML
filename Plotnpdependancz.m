clear all
close all

%% logn logp deltaacc
u =importdata('df.matlab.csv');
data_u = u.data;
figure;
scatter3(data_u(:,1),data_u(:,2),data_u(:,3),10,data_u(:,3));
xlabel('logn')
ylabel('logp')
zlabel('Difference in accuracy')
axis([0 12 0 8 -0.2 0.6])


%% 2d plot with logn
figure
scatter(data_u(:,2),data_u(:,3),20,data_u(:,1))
c = colorbar()
c.Label.String = 'logn'
ylabel('Difference in accuracy')
xlabel('logp')

%% 2d plot with logn/n = logn-logp
figure
scatter(data_u(:,2),data_u(:,3),20,-data_u(:,1)+data_u(:,2))
c = colorbar()
c.Label.String = 'logpsurn'
ylabel('Difference in accuracy')
xlabel('logp')

%% 2d plot with n and p
figure
scatter(data_u(:,1),data_u(:,2),20,data_u(:,3))
xlabel('logn')
ylabel('logp')
c = colorbar();
c.Label.String = 'Difference in acc';
axis([0 12 0 8])
